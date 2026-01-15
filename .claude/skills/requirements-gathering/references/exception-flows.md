# Exception Flows


### E1. System Unavailable
- System displays error message
- Changes are saved locally
- User can retry when system is available

### E2. Pricing Service Fails
- System displays warning message
- Sales Rep can use last known pricing
- Quote is flagged for pricing verification

### E3. Customer Credit Check Fails
- System displays credit hold warning
- Quote can be created but not submitted
- Sales Rep must contact Finance team
