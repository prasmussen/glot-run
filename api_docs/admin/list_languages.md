## Listing languages

### List languages request
    curl --request GET \
         --header 'Authorization: Token some-secret' \
         --url 'https://run.glot.io/admin/languages'


### Example response data
    [
      {
        "id": "eeecd07765ce8cb4a60e52177f8e36bf80d5dbd4",
        "image": "glot/erlang:latest",
        "name": "erlang",
        "url": "https://run.glot.io/admin/languages/eeecd07765ce8cb4a60e52177f8e36bf80d5dbd4",
        "version": "latest"
      },
      {
        "id": "c82a38621af719efa49dc0d7b88c6467cd45dc77",
        "image": "glot/haskell:latest",
        "name": "haskell",
        "url": "https://run.glot.io/admin/languages/c82a38621af719efa49dc0d7b88c6467cd45dc77",
        "version": "latest"
      }
    ]
