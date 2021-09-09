lkups <- pipapi::create_lkups(Sys.getenv('PIPAPI_DATA_ROOT_FOLDER'))

start_api(api_version = "v1",
          port = 80,
          host = "0.0.0.0")
