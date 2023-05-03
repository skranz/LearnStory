get.quizdb = function(db.dir = getApp()$glob$db.dir, reconnect=FALSE) {
  restore.point("get.quizdb")
  db.name = "quizdb.sqlite"

  db = getOption("quizdb.connection")

  if (!is.null(db)) {
    if (!dbIsValid(db)) db = NULL
  }

  if (is.null(db)) {
    db = dbConnect(RSQLite::SQLite(),file.path(db.dir, db.name))
    #db = set.db.schemas(db, schemas)
    schema.file = system.file("schema/quizdb.yaml",package = "LearnStory")
    schemas = dbmisc::load.and.init.schemas(schema.file)
    db = set.db.schemas(db, schemas)
    options(quizdb.connection = db)
  }
  db
}

