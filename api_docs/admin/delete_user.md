## Delete user

### Delete user request
    curl --request DELETE \
         --header 'Authorization: Token some-secret' \
         --url 'https://run.glot.io/admin/users/8aa7e7fc-4d9b-4f97-a950-887b55ddcf1c'

#### Response
A successful delete returns a 204 No Content, with an empty body.
