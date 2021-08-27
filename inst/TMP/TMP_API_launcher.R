lkups <- pipapi::create_lkups(Sys.getenv('PIPAPI_DATA_FOLDER_ROOT'))

start_api(api_version = "v1",
          port = 80,
          host = "0.0.0.0")
