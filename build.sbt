libraryDependencies  ++= Seq(
            "org.scalanlp" %% "breeze" % "0.8.1",
            "org.scalanlp" %% "breeze-natives" % "0.8.1",
            "org.scalatest" %% "scalatest" % "2.2.1" % "test",
            "org.scalanlp" % "breeze-viz_2.10" % "0.9"
)

resolvers ++= Seq(
            "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)
