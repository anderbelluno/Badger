<p align="center">
  <a href="https://github.com/anderbelluno/Badger/blob/main/img/Badger.png">
    <img alt="Badger" height="150" src="https://github.com/anderbelluno/Badger/blob/main/img/Badger.png">
  </a>  
</p>

# Badger

**Badger** is an open-source library designed for Delphi and Lazarus (Object Pascal) environments to help you quickly build HTTP servers and REST APIs. It includes support for features like authentication, route management, and more.

---

## ✨ Features

- **HTTP Server**: Quickly spin up HTTP servers in Delphi/Lazarus projects.
- **Route Management**: Register and protect routes for your APIs.
- **Authentication**: Built-in support for Basic Auth and JWT (JSON Web Token) authentication.
- **MIME Type Handling**: Utility functions for recognizing MIME types of files.
- **Cross-Platform**: Designed to work with both Delphi and Lazarus (FPC).
- **Logger**: Flexible and thread-safe logging via `BadgerLogger`.

---

## 📝 BadgerLogger

`BadgerLogger` is a built-in, thread-safe logging utility for Badger. It supports logging at multiple levels (Debug, Info, Warning, Error, Critical), with output options for console, file, and (on Windows) the debugger. You can configure what to log and where to log it.

### Key Features

- Multiple log levels: Debug, Info, Warning, Error, Critical.
- Thread-safe logging.
- Output to console and/or file (configurable).
- Optional output to Windows debugger (`OutputDebugString`).
- Easy to use: just call `Logger.Info('message')`, etc.

### Usage Example

```pascal
uses
  BadgerLogger;

begin
  Logger.isActive := True;
  Logger.LogToConsole := True; // Enable console output
  Logger.LogToFile := True;    // Enable file output
  Logger.LogFileName := 'server.log'; // Set log file name
  Logger.LogLevel := llDebug;  // Log everything from Debug up

  Logger.Info('Server starting...');
  Logger.Warning('Low disk space!');
  Logger.Error('Failed to bind port');
  Logger.Debug('Debug details here');
  Logger.Critical('Unexpected shutdown!');
end;
```

By default, logging to the console is enabled, and logging to file is disabled. You can adjust these behaviors at runtime using the provided properties.

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

## 💬 Telegram

Join our community on Telegram:** [https://t.me/badgerbrasil](https://t.me/badgerbrasil)

---

## ⚖️ License

Badger is free and open-source software licensed under the [MIT License](https://github.com/anderbelluno/Badger/blob/master/LICENSE).