# Shiny UI Notes

## Centering Elements with Selectize Inputs

**Problem**: A wrapper div appears off-center even though CSS centering is applied correctly.

**Root Cause**: The wrapper is centered, but selectize inputs inside don't automatically fill their parent width. They default to a narrow width (e.g., 300px) and left-align within the wrapper, making the visible input appear off-center.

**Solution**: Force all elements in the chain to be full width:

```css
.wrapper {
  position: fixed;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  width: 500px;
}

/* Force all children to fill the wrapper */
.wrapper .form-group {
  width: 100%;
}
.wrapper .form-group > div {
  width: 100%;
}
.wrapper .selectize-control {
  width: 100% !important;
}
.wrapper .selectize-input {
  width: 100% !important;
}
```

**Debugging tip**: In DevTools, check the width of the inner selectize elements, not just the wrapper. The wrapper may be centered correctly while inner elements are narrower and left-aligned.
