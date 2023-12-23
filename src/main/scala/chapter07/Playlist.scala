package chapter07

object Playlist {
  object model {
    opaque type User = String

    object User {
      def apply(value: String): User = value
    }

    opaque type Artist = String

    object Artist {
      def apply(value: String): Artist = value
    }

    case class Song(artist: Artist, title: String)

    enum MusicGenre {
      case House
      case Funk
      case HipHop
    }

    enum PlaylistKind {
      case CuratedByUser(user: User)
      case BasedOnArtist(artist: Artist)
      case BasedOnGenre(genres: Set[MusicGenre])
    }

    case class Playlist(name: String, kind: PlaylistKind, songs: List[Song])
  }

  import model.*
  import model.PlaylistKind.*

  def gatherSongs(playlists: List[Playlist], artist: Artist, genre: MusicGenre): List[Song] =
    playlists.foldLeft(List.empty[Song]) { (songs, playlist) =>
      val matchingSongs = playlist.kind match {
        case CuratedByUser(user)                                       => playlist.songs.filter { _.artist == artist }
        case BasedOnArtist(playlistArtist) if playlistArtist == artist => playlist.songs
        case BasedOnGenre(genres) if genres.contains(genre)            => playlist.songs
        case BasedOnArtist(_) | BasedOnGenre(_)                        => List.empty
      }
      songs.appendedAll(matchingSongs)
    }
}
