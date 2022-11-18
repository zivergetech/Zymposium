import zio._ 
import com.devsisters.shardcake._
import com.devsisters.shardcake.interfaces._

object ShardManagerApp extends ZIOAppDefault {

  val config = ManagerConfig.default.copy(apiPort = 8081)

  def run: Task[Nothing] =
    Server.run.provide(
      ZLayer.succeed(config),
      ZLayer.succeed(GrpcConfig.default),
      PodsHealth.local, // just ping a pod to see if it's alive
      GrpcPods.live,    // use gRPC protocol
      Storage.memory,   // store data in memory
      ShardManager.live // Shard Manager logic
    )

}