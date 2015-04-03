#!/bin/bash
set -o nounset

sqlite3 "${SAVED_BINARIES}/info.sqlite" <<SQL
	CREATE TABLE IF NOT EXISTS  "Versions" (
	"name" TEXT NOT NULL,
	"hash" TEXT NOT NULL,
	"buildDate" TEXT,
	"scm" TEXT NOT NULL,
	"id" INTEGER NOT NULL UNIQUE ,	
	PRIMARY KEY ("name", "hash")
	);
	
	CREATE TABLE IF NOT EXISTS  "Hosts" (
	"hostType" TEXT NOT NULL,	
	"binId" INTEGER NOT NULL UNIQUE,	
	"id" INTEGER NOT NULL UNIQUE,
	PRIMARY KEY ("hostType", "binId"),
	FOREIGN KEY(binId) REFERENCES Versions(id)
	);
	
	CREATE TABLE IF NOT EXISTS  "Groups" (
	"id" INTEGER NOT NULL PRIMARY KEY,
	"hostType" TEXT NOT NULL,
	"filePath" Text NOT NULL UNIQUE,
	FOREIGN KEY(hostType) REFERENCES Hosts(hostType)
	);
	
	CREATE TABLE IF NOT EXISTS  "GroupItems" (
	"groupId" INTEGER NOT NULL,	
	"binId"  INTEGER NOT NULL,
	"id" INTEGER NOT NULL UNIQUE,
	PRIMARY KEY ("groupId", "binId"),
	FOREIGN KEY(binId) REFERENCES Versions(id)
	FOREIGN KEY(groupId) REFERENCES Groups(id)
	);
	
	
	CREATE VIEW IF NOT EXISTS "EquivalentGroups"  as
	Select Src.id as srcId, Src.filePath as srcFilePath, Src.hostType as srcHostType,
           Dst.id as dstId, Dst.filePath as dstFilePath, Dst.hostType as dstHostType,
           Dst.bins as bins
	From Groups Src

	Join (
		Select G.id, G.filePath, G.hostType, I.bins 
        From Groups G

		Join (
			Select group_concat(Gi.binId) As bins , groupId
			From GroupItems Gi 
			Group By Gi.groupId 
		) I On I.groupId = G.id 

	) Dst Where Dst.id != Src.id And Dst.bins = 
		(Select bins from (   
		    Select group_concat(Gi.binId) As bins , groupId
			From GroupItems Gi 
			Group By Gi.groupId 
		) O Where O.groupId =  Src.id
		)  AND srcHostType != dstHostType
	Order by srcId, srcHostType, dstHostType;

	CREATE VIEW IF NOT EXISTS "EquivalentToolchain"  as
	Select Src.id as srcId, Src.filePath as srcFilePath, Src.hostType as srcHostType,
           Dst.id as dstId, Dst.filePath as dstFilePath, Dst.hostType as dstHostType,
           Dst.bins as bins
	From Groups Src

	Join (
		Select G.id, G.filePath, G.hostType, I.bins 
        From Groups G

	    Join (
			Select group_concat(Gi.binId) As bins , groupId
			From GroupItems Gi 
            Join Versions V 
            On Gi.binId = V.id
            where V.name not in  ("testSample", "testReduce", "gen")
			Group By Gi.groupId 
		) I On I.groupId = G.id 

	) Dst Where Dst.id != Src.id And Dst.bins = 
		(Select bins from (   
		    Select group_concat(Gi.binId) As bins , groupId
			From GroupItems Gi 
			Join Versions V 
            On Gi.binId = V.id
            where V.name not in  ("testSample", "testReduce", "gen")
			Group By Gi.groupId  
		) O Where O.groupId =  Src.id
		) AND srcHostType != dstHostType
	Order by srcId, srcHostType, dstHostType;

	
	CREATE VIEW IF NOT EXISTS "EquivalentConjure"  as
	Select Src.id as srcId, Src.filePath as srcFilePath, Src.hostType as srcHostType,
           Dst.id as dstId, Dst.filePath as dstFilePath, Dst.hostType as dstHostType
	From Groups Src

	Join (
		Select G.id, G.filePath, G.hostType, I.bins 
        From Groups G

	    Join (
			Select group_concat(Gi.binId) As bins , groupId
			From GroupItems Gi 
            Join Versions V 
            On Gi.binId = V.id
            where V.name in  ("conjureNew", "conjureOld")
			Group By Gi.groupId 
		) I On I.groupId = G.id 

	) Dst Where Dst.id != Src.id And Dst.bins = 
		(Select bins from (   
		    Select group_concat(Gi.binId) As bins , groupId
			From GroupItems Gi 
			Join Versions V 
            On Gi.binId = V.id
            where V.name in  ("conjureNew", "conjureOld")
			Group By Gi.groupId  
		) O Where O.groupId =  Src.id
		) AND srcHostType != dstHostType
	Order by srcId, srcHostType, dstHostType;

	CREATE VIEW IF NOT EXISTS "GenGroup"  as
	Select 
	id as groupId, 
	( Select hash from Groups G 
	  Join GroupItems Gi ON Gi.groupId = g.id
	  Join Versions V ON Gi.binId = V.id 
	  where name = 'gen' and G.id = H.id ) as gen,

	( Select hash from Groups G 
	  Join GroupItems Gi ON Gi.groupId = g.id
	  Join Versions V ON Gi.binId = V.id 
	  where name = 'conjureNew' and G.id = H.id ) as conjureNew,

	( Select hash from Groups G 
	  Join GroupItems Gi ON Gi.groupId = g.id
	  Join Versions V ON Gi.binId = V.id 
	  where name = 'savilerow' and G.id = H.id ) as savilerow,

	( Select hash from Groups G 
	  Join GroupItems Gi ON Gi.groupId = g.id
	  Join Versions V ON Gi.binId = V.id 
	  where name = 'minion' and G.id = H.id ) as minion,
	  
	H.filePath

	From Groups H
	Where gen is not NULL
	Order by H.filePath;
	
	CREATE VIEW IF NOT EXISTS "ToolchainGroup"  as
	Select 
	id as groupId,
	( Select hash from Groups G 
	  Join GroupItems Gi ON Gi.groupId = g.id
	  Join Versions V ON Gi.binId = V.id 
	  where name = 'conjureNew' and G.id = H.id ) as conjureNew,

	( Select hash from Groups G 
	  Join GroupItems Gi ON Gi.groupId = g.id
	  Join Versions V ON Gi.binId = V.id 
	  where name = 'savilerow' and G.id = H.id ) as savilerow,

	( Select hash from Groups G 
	  Join GroupItems Gi ON Gi.groupId = g.id
	  Join Versions V ON Gi.binId = V.id 
	  where name = 'minion' and G.id = H.id ) as minion,
	  
	H.filePath

	From Groups H
	Order by H.filePath;	
	
SQL