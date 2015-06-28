## Get language

### Get language request
    curl --request GET \
         --header 'Authorization: Token some-secret' \
         --url 'https://run.glot.io/admin/languages/8aa7e7fc-4d9b-4f97-a950-887b55ddcf1c'


### Example response data
    {
      "id": "eeecd07765ce8cb4a60e52177f8e36bf80d5dbd4",
      "image": "glot/erlang:latest",
      "name": "erlang",
      "version": "latest"
    }
