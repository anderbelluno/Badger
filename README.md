<p align="center">
  <a href="https://github.com/anderbelluno/Badger/blob/main/img/Badger.png">
    <img alt="Badger" height="150" src="https://github.com/anderbelluno/Badger/blob/main/img/Badger.png">
  </a>  
</p>

# Badger

**Badger** is an open-source library designed for Delphi and Lazarus (Object Pascal) environments to help you quickly build HTTP servers and REST APIs. It includes support for features like authentication (Basic and JWT), route management, MIME type handling, and more, making it ideal for developing modern web backends or embedded HTTP services.

---

## ✨ Features

- **HTTP Server**: Quickly spin up HTTP servers in Delphi/Lazarus projects.
- **Route Management**: Register and protect routes for your APIs.
- **Authentication**: Built-in support for Basic Auth and JWT (JSON Web Token) authentication.
- **MIME Type Handling**: Utility functions for recognizing MIME types of files.
- **Cross-Platform**: Designed to work with both Delphi and Lazarus (FPC).

---

## 🚀 Getting Started

### 1. Clone the Repository

```sh
git clone --recursive https://github.com/anderbelluno/Badger.git
```

Or, if you already cloned without submodules:

```sh
git submodule update --init --recursive
```

### 2. Dependencies

All dependencies are managed as git submodules and are listed in the **.gitmodules** file. The main third-party dependency is:

- [Synapse](https://github.com/geby/synapse) – for networking functionality.

If you want to download dependencies manually, place them in the `ThirdParty` folder.

---

## 🧩 Usage Example

Here's a simplified example of spinning up a Badger HTTP server in a Lazarus project:

```pascal
ServerThread := TBadger.Create;
ServerThread.Port := 8080;
ServerThread.Timeout := 5000;
ServerThread.NonBlockMode := True;

BasicAuth := TBasicAuth.Create('username', 'password');
JWTAuth := TBadgerJWTAuth.Create('secretkey', 'c:\\tokens'); // Token storage

// Register protected routes
BasicAuth.RegisterProtectedRoutes(ServerThread, ['/route1', '/ping', '/download']);
JWTAuth.RegisterProtectedRoutes(ServerThread, ['/route1', '/ping']);

ServerThread.OnRequest  := HandleRequest;
ServerThread.OnResponse := HandleResponse;

ServerThread.Start;
```

See `sample/Lazarus/unit1.pas` and `sample/D7/Unit1.pas` for complete working examples.

---

## 🛠️ Project Structure

- `src/` — Main library source code
- `sample/` — Example projects for both Delphi (D7) and Lazarus
- `img/` — Project images and logos

---

## 👥 Code Contributors

<a href="https://github.com/anderbelluno/Badger/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=anderbelluno/Badger" />
</a>

---

## ⚖️ License

Badger is free and open-source software licensed under the [MIT License](https://github.com/anderbelluno/Badger/blob/master/LICENSE).
