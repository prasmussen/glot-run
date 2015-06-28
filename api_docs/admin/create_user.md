## Create user

### Create user request
    curl --request POST \
         --header 'Authorization: Token some-secret' \
         --header 'Content-type: application/json' \
         --data '{"token": "d11088bc-a29d-4d49-a633-b1b1ae807064"}' \
         --url 'https://run.glot.io/admin/users'


### Example response data
    {
      "created": "2015-06-28T18:40:52Z",
      "id": "8aa7e7fc-4d9b-4f97-a950-887b55ddcf1c",
      "modified": "2015-06-28T18:40:52Z",
      "token": "d11088bc-a29d-4d49-a633-b1b1ae807064",
      "url": "https://run.glot.io/admin/users/8aa7e7fc-4d9b-4f97-a950-887b55ddcf1c"
    }
