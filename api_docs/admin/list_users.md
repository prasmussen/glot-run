## Listing users

### List users request
    curl --request GET \
         --header 'Authorization: Token some-secret' \
         --url 'https://run.glot.io/admin/users'


### Example response data
    [
      {
        "created": "2015-04-19T22:22:09Z",
        "id": "5b48c546-8e30-4ca6-ade0-e7e8765e71bc",
        "modified": "2015-04-19T22:22:09Z",
        "token": "ffb05492-258d-4dd6-9713-eee722874ff6",
        "url": "https://run.glot.io/admin/users/5b48c546-8e30-4ca6-ade0-e7e8765e71bc"
      },
      {
        "created": "2015-04-19T22:00:04Z",
        "id": "6165b155-6133-4cf5-b8b5-0f675f221257",
        "modified": "2015-04-19T22:00:04Z",
        "token": "d1c31bd3-9afe-4ee6-8ea6-5db9ff1375ef",
        "url": "https://run.glot.io/admin/users/6165b155-6133-4cf5-b8b5-0f675f221257"
      }
    ]
