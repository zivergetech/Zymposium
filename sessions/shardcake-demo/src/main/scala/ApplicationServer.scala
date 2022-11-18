import zio._
import sttp.client3.UriContext
import sttp.model.Uri
import com.devsisters.shardcake._
import com.devsisters.shardcake.interfaces._

object ApplicationServer {

  sealed trait GuildMessage

  object GuildMessage {
    final case class Join(user: String, replier: Replier[Unit])  extends GuildMessage
    final case class Leave(user: String, replier: Replier[Unit]) extends GuildMessage
    final case class GetMembers(replier: Replier[Set[String]])   extends GuildMessage
  }

  object Guild extends EntityType[GuildMessage]("guild")

  def behavior(entityId: String, messages: Dequeue[GuildMessage]): RIO[Sharding, Nothing] =
    Ref.make(Set.empty[String]).flatMap { state =>
      messages.take.flatMap {
        case GuildMessage.Join(user, replier)  => state.update(_ + user) *> replier.reply(())
        case GuildMessage.GetMembers(replier)  => state.get.debug.flatMap(replier.reply)
        case GuildMessage.Leave(user, replier) => state.update(_ - user) *> replier.reply(())
      }.forever
    }

  val program =
    for {
      _       <- Sharding.registerEntity(Guild, behavior)
      _       <- Sharding.registerScoped
      guild   <- Sharding.messenger(Guild)
      _       <- guild.send("guild1")(GuildMessage.Join("user1", _))
      _       <- guild.send("guild1")(replier => GuildMessage.Join("user2", replier))
      members <- guild.send("guild1")(GuildMessage.GetMembers(_))
      _       <- ZIO.debug(members)
      _       <- ZIO.never
    } yield ()

  val config = Config.default.copy(shardingPort = 5555, shardManagerUri = uri"http://localhost:8081/api/graphql")
  val run =
    ZIO
      .scoped(program)
      .provide(
        ZLayer.succeed(config),
        ZLayer.succeed(GrpcConfig.default),
        Serialization.javaSerialization,
        Storage.memory,
        ShardManagerClient.liveWithSttp,
        GrpcPods.live,
        Sharding.live,
        GrpcShardingService.live
      )
}
