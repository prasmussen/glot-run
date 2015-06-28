## Update user

### Update user token request
    curl --request PUT \
         --header 'Authorization: Token some-secret' \
         --header 'Content-type: application/json' \
         --data '{"token": "62c8e937-780f-4ca8-ba93-168057263afe"}' \
         --url 'https://run.glot.io/admin/users/8aa7e7fc-4d9b-4f97-a950-887b55ddcf1c'


### Example response data
    {
      "created": "2015-06-28T18:40:52Z",
      "id": "8aa7e7fc-4d9b-4f97-a950-887b55ddcf1c",
      "modified": "2015-06-28T18:40:52Z",
      "token": "62c8e937-780f-4ca8-ba93-168057263afe",
      "url": "https://run.glot.io/admin/users/8aa7e7fc-4d9b-4f97-a950-887b55ddcf1c"
    }
