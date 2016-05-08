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

Note: the first file in the array will be the one executed. The rest of the files can be supporting files, such as
modules the first file uses.

### Example response data
    {
      "stdout": "42\n",
      "stderr": "",
      "error": ""
    }
