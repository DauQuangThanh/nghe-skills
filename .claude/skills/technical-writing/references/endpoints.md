# Endpoints


### GET /api/resource
- Description: What this endpoint does
- Parameters:
  - `param1` (string, required): Description
  - `param2` (integer, optional): Description
- Request Example:
  ```http
  GET /api/resource?param1=value HTTP/1.1
  Authorization: Bearer <token>
  ```
- Response Example:
  ```json
  {
    "status": "success",
    "data": {...}
  }
  ```
- Error Responses:
  - 400: Bad Request - Invalid parameters
  - 401: Unauthorized - Missing or invalid token
  - 404: Not Found - Resource does not exist
