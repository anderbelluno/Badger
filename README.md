<p align="center">
  <a href="https://github.com/anderbelluno/Badger/blob/main/img/Badger.png">
    <img alt="Badger" height="150" src="https://github.com/anderbelluno/Badger/blob/main/img/Badger.png">
  </a>  
</p>

# Badger

---

**Badger** is an open-source library designed for Delphi and Lazarus (Object Pascal) environments to help you quickly build HTTP servers and REST APIs. It includes support for features like authentication, routing, CORS, logging and more.

---

🦡⚡ Challenge: this project delivers extremely high throughput for simple HTTP endpoints. Think it's fast? Test it yourself — beat these numbers and open a PR with your results. Let's see who can outperform Badger.

## Performance Benchmark (summary — highlighted)
- Throughput: **30,483 req/s**
- Average latency: **4 ms**
- Median latency: **0 ms**
- 90th percentile: **16 ms**
- 95th percentile: **27 ms**
- 99th percentile: **42 ms**
- Errors: **0%**

> Important: the tests were performed with **Keep-Alive = true** (persistent connections). This significantly reduces connection setup overhead and is a critical factor contributing to the high throughput observed.

![JMeter Benchmark](https://github.com/anderbelluno/Badger/blob/main/img/JMeter_benchmark.png?raw=true)

## ✨ Features

- **HTTP Server**: Quickly spin up HTTP servers in Delphi/Lazarus projects.
- **Route Management**: Register and protect routes for your APIs.
- **Authentication**: Built-in support for Basic Auth and JWT (JSON Web Token) authentication.
- **CORS (Cross-Origin Resource Sharing)**: Configurable CORS support with options for allowed origins, methods, headers, credentials and automatic preflight (OPTIONS) handling.
- **MIME Type Handling**: Utility functions for recognizing MIME types of files.
- **Cross-Platform**: Designed to work with both Delphi and Lazarus (FPC).
- **Logger**: Flexible and thread-safe logging via `BadgerLogger`.

---

## 🌐 CORS Support

Badger now includes built-in support for Cross-Origin Resource Sharing (CORS). This lets you control which origins, HTTP methods and headers are allowed when clients from other domains access your[...]

Usage example (illustrative):

```pascal
// Enable and configure CORS (API names are illustrative — adapt to actual API in code)
ServerThread.EnableCORS := True;
ServerThread.CORS.AllowOrigins := ['*']; // or specific origins like ['https://example.com']
ServerThread.CORS.AllowMethods := ['GET','POST','PUT','DELETE','OPTIONS'];
ServerThread.CORS.AllowHeaders := ['Content-Type','Authorization'];
ServerThread.CORS.AllowCredentials := False;

// Alternatively you can set Access-Control-* headers in your OnResponse handler:
procedure HandleResponse(Sender: TObject; Request: TBadgerRequest; Response: TBadgerResponse);
begin
  Response.SetHeader('Access-Control-Allow-Origin','*');
  Response.SetHeader('Access-Control-Allow-Methods','GET,POST,PUT,DELETE,OPTIONS');
  Response.SetHeader('Access-Control-Allow-Headers','Content-Type,Authorization');
end;
```

Note: adjust the example to match the exact property/method names used in your Badger fork — the README demonstrates the intended configuration and behavior.

Preflight (OPTIONS): when CORS is enabled Badger will automatically reply to OPTIONS/preflight requests with the appropriate Access-Control-* headers. If your application implements custom logic f[...]

---

## 📝 BadgerLogger

`BadgerLogger` is a built-in, thread-safe logging utility for Badger. It supports logging at multiple levels (Debug, Info, Warning, Error, Critical), with output options for console, file, and (on[...]

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
JWTAuth := TBadgerJWTAuth.Create('secretkey', 'c:\tokens'); // Token storage

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
