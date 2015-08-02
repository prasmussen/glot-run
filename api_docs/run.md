## Run code

##### Run code
    curl --request POST \
         --header 'Authorization: Token 0123456-789a-bcde-f012-3456789abcde' \
         --header 'Content-type: application/json' \
         --data '{"files": [{"name": "main.py", "content": "print(42)"}]}' \
         --url 'https://run.glot.io/languages/python/latest'

### Example request data
    {
      "files": [
        {
          "name": "main.py",
          "content": "print(42)"
        }
      ]
    }


### Example response data
    {
      "stdout": "42\n",
      "stderr": "",
      "error": ""
    }
