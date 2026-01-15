# Alternative Flows


### 3a. Product Not in Catalog
3a1. Sales Rep searches for product
3a2. System returns no results
3a3. Sales Rep clicks "Request New Product"
3a4. System opens product request form
3a5. Sales Rep submits product request
3a6. System notifies product team
3a7. Sales Rep can save quote as draft and return later
→ Use case continues at step 9

### 8a. Special Pricing Required
8a1. Sales Rep applies discount > standard threshold
8a2. System flags quote as requiring special approval
8a3. System requires justification from Sales Rep
8a4. Sales Rep enters discount justification
→ Use case continues at step 9

### 10a. Quote Validation Fails
10a1. System identifies missing required fields
10a2. System displays error messages
10a3. Sales Rep corrects errors
→ Use case continues at step 10

### 13a. Auto-Approval
13a1. System determines quote meets auto-approval criteria:
     - Total value < $10,000
     - Discount < 10%
     - Standard customer terms
13a2. System auto-approves quote
13a3. System notifies Sales Rep
13a4. System generates final quote PDF
→ Use case ends successfully
