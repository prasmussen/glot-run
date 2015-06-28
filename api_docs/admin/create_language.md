## Create language

### Create language request
    curl --request PUT \
         --header 'Authorization: Token some-secret' \
         --header 'Content-type: application/json' \
         --data '{"name": "erlang", "version": "latest", "image": "glot/erlang:latest"}' \
         --url 'https://run.glot.io/admin/languages'


### Example response data
    {
      "id": "eeecd07765ce8cb4a60e52177f8e36bf80d5dbd4",
      "image": "glot/erlang:latest",
      "name": "erlang",
      "url": "https://run.glot.io/admin/languages/eeecd07765ce8cb4a60e52177f8e36bf80d5dbd4",
      "version": "latest"
    }
