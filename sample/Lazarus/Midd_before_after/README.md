# Middleware Before / After sample

GUI no estilo de `sample/Lazarus/GUI`.

## Open

`MiddBeforeAfter.lpi`

## O que demonstra

| Peça | Quem |
|------|------|
| **Before** | `TBasicAuth.RegisterProtectedRoutes(..., ['/ping'])` |
| **Rota** | `GET /ping` → `{"ok":true,"message":"pong"}` |
| **After** | `TJsonEnvelopeAfter.RegisterProtectedRoutes(..., ['/ping'])` — SuperObject |

Mesmo padrão nos dois: a lista de rotas define onde cada middleware age.

```pascal
BasicAuth.RegisterProtectedRoutes(ServerThread, ['/ping']);
JsonAfter.RegisterProtectedRoutes(ServerThread, ['/ping']);
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
