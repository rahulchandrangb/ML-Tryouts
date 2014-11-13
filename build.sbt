libraryDependencies  ++= Seq(
            "org.scalanlp" %% "breeze" % "0.8.1",
            "org.scalanlp" %% "breeze-natives" % "0.8.1"
)

resolvers ++= Seq(
            "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)
