## Run code

##### Run code
    curl --request POST \
         --header 'Authorization: Token 0123456-789a-bcde-f012-3456789abcde' \
         --header 'Content-type: application/json' \
         --data '[{"name": "main.py", "content": "print(42)"}]' \
         --url 'https://run.glot.io/languages/python/latest'

### Example response
    {
      "stdout": "42\n",
      "stderr": "",
      "error": ""
    }
