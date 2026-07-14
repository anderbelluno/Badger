# Middleware Before / After sample

GUI no estilo de `sample/Lazarus/GUI`.

## Open

`MiddBeforeAfter.lpi`

## O que demonstra

| Peça | Quem |
|------|------|
| **Before** | `TBasicAuth` — `RegisterProtectedRoutes(..., ['/ping'])` |
| **Rota** | `GET /ping` → `{"ok":true,"message":"pong"}` |
| **After** | `AddAfterMiddleware(AfterJsonEnvelope)` — envelopa o body com SuperObject |

Com **Basic + After JSON** selecionado:

```pascal
BasicAuth.RegisterProtectedRoutes(ServerThread, ['/ping']);
ServerThread.AddAfterMiddleware(AfterJsonEnvelope);
ServerThread.RouteManager.AddGet('/ping', TDemoRoutes.Ping);
```

Credenciais: `username` / `password` (iguais ao sample GUI).

## Teste

```bash
# 401 sem auth
curl -i http://127.0.0.1:8090/ping

# 200 + envelope meta (after)
curl -u username:password http://127.0.0.1:8090/ping
```

Resposta esperada (com middlewares):

```json
{
  "data": {"ok": true, "message": "pong"},
  "meta": {"method": "GET", "uri": "/ping", "status": 200}
}
```
