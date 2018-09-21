namespace AltCover.Test

open System
open System.ComponentModel
open System.Configuration.Install
open System.Diagnostics
open System.ServiceProcess
open System.Threading

[<RunInstaller(true)>]
type ProjectInstaller() as this =
  inherit System.Configuration.Install.Installer()
  do
    //
    // serviceProcessInstaller
    //
    let serviceProcessInstaller = new ServiceProcessInstaller()
    serviceProcessInstaller.Account <- ServiceAccount.NetworkService
    serviceProcessInstaller.Password <- null
    serviceProcessInstaller.Username <- null
    //
    // serviceInstaller
    //
    let serviceInstaller = new ServiceInstaller()
    serviceInstaller.Description <- "Test service for coverage"
    serviceInstaller.DisplayName <- "AltCover Test Service"
    serviceInstaller.ServiceName <- "AltCover.Test.Service"
    serviceInstaller.StartType <- ServiceStartMode.Automatic
    //
    // ProjectInstaller
    //
    this.Installers.AddRange([| serviceProcessInstaller :> Installer
                                serviceInstaller :> Installer |])

/// <summary>
/// Trival service based on the walkthrough at http://msdn.microsoft.com/en-us/library/zt39148a%28v=vs.110%29.aspx
/// Uses Debug.WriteLine rather than Event Log to avoid the need to set access rights for the relevant keys
/// Follow the service activity by running DebugView -- http://technet.microsoft.com/en-gb/sysinternals/bb896647.aspx -- as
/// Administrator and Capture Global Win32 traces
/// </summary>
type Service() as this =
  inherit ServiceBase()
  do this.Initialize()
  member private this.waiter = new AutoResetEvent(false)

  member this.Initialize() =
    this.ServiceName <- "AltCoverSvc"
    this.CanShutdown <- true
    this.CanStop <- true
    Debug.WriteLine "Constructed service"

  /// <summary>
  /// Called by the SCM when a service start is sent and starts
  /// the service payload as an asynchronous operation
  /// </summary>
  /// <param name="args">This parameter is not used.</param>
  override this.OnStart args =
    Debug.WriteLine("Starting service")
    let sleepWorkflow =
      async {
        let interval = TimeSpan(0, 0, 5)

        let rec loop() =
          if this.waiter.WaitOne(interval) then Debug.WriteLine "Service exiting"
          else
            Debug.WriteLine "Service working"
            loop()
        Debug.WriteLine "Starting sleep workflow"
        loop()
      }
    Async.Start sleepWorkflow

  /// <summary>
  /// Called by the SCM when a service stop is sent
  /// </summary>
  override this.OnStop() =
    Debug.WriteLine "Stopping service"
    this.waiter.Set() |> ignore

  /// <summary>
  /// Called by the SCM when the system is shutting down
  /// </summary>
  override this.OnShutdown() =
    Debug.WriteLine "Shutting down service"
    this.waiter.Set() |> ignore

module Control =
  [<EntryPoint>]
  let Main argv =
    ServiceBase.Run [| (new Service()) :> ServiceBase |]
    0 // return an integer exit code