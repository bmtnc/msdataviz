# Deploying Fundamentals Explorer on Mac Mini

This guide walks through deploying the Shiny app on a Mac Mini with public access via Cloudflare Tunnel.

**Estimated cost: ~$1/month** (domain registration only)

## Architecture

```
Internet
    │
    ▼
┌─────────────────┐
│ Cloudflare      │  (DDoS protection, SSL, CDN)
│ Edge Network    │
└────────┬────────┘
         │ (encrypted tunnel)
         ▼
┌─────────────────┐
│ Mac Mini        │
│ ┌─────────────┐ │
│ │ cloudflared │ │  (tunnel daemon)
│ └──────┬──────┘ │
│        │        │
│ ┌──────▼──────┐ │
│ │   Docker    │ │
│ │ Shiny App   │ │  (port 3838)
│ └─────────────┘ │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ S3 Bucket       │  (avpipeline-artifacts-prod)
└─────────────────┘
```

## Prerequisites

- Mac Mini with macOS
- Homebrew installed (`/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"`)
- AWS credentials with S3 read access to `avpipeline-artifacts-prod`

---

## Step 1: Install Docker Desktop

1. Download Docker Desktop from https://www.docker.com/products/docker-desktop/
2. Install and launch Docker Desktop
3. In Docker Desktop preferences:
   - Enable "Start Docker Desktop when you log in"
   - Allocate at least 4GB memory (Settings → Resources)

Verify installation:
```bash
docker --version
docker compose version
```

---

## Step 2: Clone and Build the App

```bash
# Clone the repository
cd ~/Projects  # or your preferred directory
git clone https://github.com/bmtnc/msdataviz.git
cd msdataviz

# Build the Docker image (takes ~15-30 minutes first time)
docker compose build
```

---

## Step 3: Configure AWS Credentials

Create a `.env` file in the msdataviz directory:

```bash
# Create .env file (this file is gitignored)
cat > .env << 'EOF'
AWS_ACCESS_KEY_ID=your_access_key_here
AWS_SECRET_ACCESS_KEY=your_secret_key_here
AWS_REGION=us-east-1
S3_BUCKET=avpipeline-artifacts-prod
EOF
```

**Important:** Never commit `.env` to git. It's already in `.gitignore`.

---

## Step 4: Test Locally

```bash
# Start the container
docker compose up -d

# Check it's running
docker compose ps

# View logs
docker compose logs -f shiny
```

Open http://localhost:3838 in your browser. You should see the Fundamentals Explorer app.

To stop:
```bash
docker compose down
```

---

## Step 5: Register a Domain

**Option A: Cloudflare Registrar (recommended)**
1. Create a Cloudflare account at https://cloudflare.com
2. Go to "Domain Registration" → "Register Domains"
3. Search for your desired domain (e.g., `yourname-finance.com`)
4. Purchase (~$10-15/year for .com)

**Option B: External Registrar (Porkbun, Namecheap)**
1. Purchase domain from registrar
2. In Cloudflare, add your domain as a site
3. Update your registrar's nameservers to point to Cloudflare's nameservers

---

## Step 6: Install Cloudflare Tunnel

```bash
# Install cloudflared via Homebrew
brew install cloudflared

# Verify installation
cloudflared --version
```

---

## Step 7: Create and Configure Tunnel

### Authenticate with Cloudflare

```bash
cloudflared tunnel login
```

This opens a browser window. Select your domain and authorize.

### Create the Tunnel

```bash
# Create a tunnel named "msdataviz"
cloudflared tunnel create msdataviz
```

This creates a credentials file at `~/.cloudflared/<TUNNEL_ID>.json`. Note the Tunnel ID output.

### Create Tunnel Configuration

```bash
# Create config file
mkdir -p ~/.cloudflared
cat > ~/.cloudflared/config.yml << 'EOF'
tunnel: msdataviz
credentials-file: /Users/YOUR_USERNAME/.cloudflared/TUNNEL_ID.json

ingress:
  - hostname: yourdomain.com
    service: http://localhost:3838
  - hostname: www.yourdomain.com
    service: http://localhost:3838
  - service: http_status:404
EOF
```

**Replace:**
- `YOUR_USERNAME` with your Mac username
- `TUNNEL_ID` with the ID from tunnel creation
- `yourdomain.com` with your actual domain

### Create DNS Record

```bash
# Route your domain to the tunnel
cloudflared tunnel route dns msdataviz yourdomain.com
cloudflared tunnel route dns msdataviz www.yourdomain.com
```

---

## Step 8: Test the Tunnel

```bash
# Start the tunnel (foreground for testing)
cloudflared tunnel run msdataviz
```

Open https://yourdomain.com in your browser. You should see the app with HTTPS!

Press Ctrl+C to stop the tunnel.

---

## Step 9: Configure Auto-Start

### Docker Auto-Start

Docker Compose `restart: unless-stopped` ensures containers restart after reboot.

Just make sure Docker Desktop is set to start on login (Step 1).

### Cloudflare Tunnel Auto-Start

Install cloudflared as a macOS service:

```bash
# Install as a LaunchAgent (runs as your user)
cloudflared service install

# Start the service
brew services start cloudflared
```

Alternatively, create a LaunchAgent manually:

```bash
cat > ~/Library/LaunchAgents/com.cloudflare.cloudflared.plist << 'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>com.cloudflare.cloudflared</string>
    <key>ProgramArguments</key>
    <array>
        <string>/opt/homebrew/bin/cloudflared</string>
        <string>tunnel</string>
        <string>run</string>
        <string>msdataviz</string>
    </array>
    <key>RunAtLoad</key>
    <true/>
    <key>KeepAlive</key>
    <true/>
    <key>StandardOutPath</key>
    <string>/tmp/cloudflared.log</string>
    <key>StandardErrorPath</key>
    <string>/tmp/cloudflared.error.log</string>
</dict>
</plist>
EOF

# Load the service
launchctl load ~/Library/LaunchAgents/com.cloudflare.cloudflared.plist
```

---

## Step 10: Verify Everything Works

1. **Reboot your Mac Mini**

2. **Wait 2-3 minutes** for Docker and cloudflared to start

3. **Check services are running:**
   ```bash
   # Check Docker container
   docker compose ps

   # Check cloudflared
   launchctl list | grep cloudflared
   ```

4. **Visit your domain** from another device (phone, different computer)

---

## Maintenance

### View Logs

```bash
# Docker/Shiny logs
docker compose logs -f shiny

# Cloudflare tunnel logs
tail -f /tmp/cloudflared.log
```

### Update the App

```bash
cd ~/Projects/msdataviz

# Pull latest code
git pull

# Rebuild container
docker compose build

# Restart with new image
docker compose up -d
```

### Restart Services

```bash
# Restart Docker container
docker compose restart

# Restart cloudflared
launchctl stop com.cloudflare.cloudflared
launchctl start com.cloudflare.cloudflared
```

---

## Troubleshooting

### App not loading locally

```bash
# Check container is running
docker compose ps

# Check logs for errors
docker compose logs shiny

# Ensure AWS credentials are set
docker compose exec shiny env | grep AWS
```

### Tunnel not connecting

```bash
# Check tunnel status
cloudflared tunnel info msdataviz

# Test tunnel manually
cloudflared tunnel run msdataviz

# Check DNS records in Cloudflare dashboard
```

### "502 Bad Gateway" from Cloudflare

- Docker container isn't running
- Port mismatch in config.yml
- Run `docker compose up -d` to start container

### Slow initial load

First load after container restart fetches data from S3 (~30 seconds). Subsequent loads use cached data.

---

## Security Notes

- **No port forwarding required** - Cloudflare Tunnel creates an outbound connection
- **Your IP is hidden** - Cloudflare proxies all traffic
- **Free DDoS protection** - Cloudflare shields your Mac Mini
- **Free SSL** - Automatic HTTPS via Cloudflare

### Optional: Add Authentication

For private access, add Cloudflare Access (free for up to 50 users):

1. Cloudflare Dashboard → Zero Trust → Access → Applications
2. Add application → Self-hosted
3. Set your domain
4. Configure authentication (email OTP, Google, etc.)

---

## Cost Summary

| Component | Cost |
|-----------|------|
| Mac Mini | $0 (already owned) |
| Docker Desktop | $0 |
| Cloudflare account | $0 |
| Cloudflare Tunnel | $0 |
| SSL certificate | $0 |
| .com domain | ~$10-15/year |
| **Total** | **~$1/month** |
