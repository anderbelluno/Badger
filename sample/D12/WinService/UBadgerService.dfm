object FBadgerService: TFBadgerService
  OnDestroy = ServiceDestroy
  DisplayName = 'BadgerService'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 480
  Width = 640
end
