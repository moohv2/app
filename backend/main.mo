import Map "mo:core/Map";
import List "mo:core/List";
import Blob "mo:core/Blob";
import Text "mo:core/Text";
import Time "mo:core/Time";
import Nat "mo:core/Nat";
import Int "mo:core/Int";
import Array "mo:core/Array";
import Principal "mo:core/Principal";
import Iter "mo:core/Iter";
import Runtime "mo:core/Runtime";
import Migration "migration";
import MixinAuthorization "authorization/MixinAuthorization";
import MixinStorage "blob-storage/Mixin";
import Storage "blob-storage/Storage";
import AccessControl "authorization/access-control";

(with migration = Migration.run)
actor {
  include MixinStorage();

  type DocumentComment = {
    id : Nat;
    documentId : Nat;
    author : Principal;
    text : Text;
    timestamp : Time.Time;
  };

  public type UserProfile = {
    name : Text;
    profilePicture : ?Storage.ExternalBlob;
  };

  public type Channel = {
    id : Nat;
    name : Text;
    createdBy : Principal;
  };

  public type MembersChannel = {
    id : Nat;
    name : Text;
    createdBy : Principal;
  };

  public type Message = {
    id : Nat;
    channelId : Nat;
    author : Text;
    content : Text;
    timestamp : Int;
    image : ?Storage.ExternalBlob;
    gif : ?Text;
    replyToId : ?Nat;
  };

  public type Sticker = {
    id : Nat;
    image : Storage.ExternalBlob;
    name : Text;
    messageId : ?Nat;
    sender : ?Text;
    channelId : ?Nat;
    timestamp : ?Int;
  };

  public type SessionMember = {
    id : Principal;
    nickname : Text;
    joinedAt : Int;
  };

  public type Session = {
    id : Nat;
    name : Text;
    host : Principal;
    passwordHash : ?Blob;
    members : [SessionMember];
    channels : [Channel];
    membersChannels : [MembersChannel];
    createdAt : Int;
    lastActive : Int;
  };

  public type Document = {
    id : Nat;
    sessionId : Nat;
    name : Text;
    content : Text;
    revision : Nat;
    locked : Bool;
    createdBy : Principal;
    lastModified : Int;
  };

  type DocumentWithImages = {
    id : Nat;
    sessionId : Nat;
    name : Text;
    content : Text;
    revision : Nat;
    locked : Bool;
    createdBy : Principal;
    lastModified : Int;
    images : [ImageReference];
  };

  public type PlayerDocument = {
    id : Nat;
    sessionId : Nat;
    owner : Principal;
    name : Text;
    content : Text;
    createdBy : Principal;
    lastModified : Int;
    images : [ImageReference];
    isPrivate : Bool;
  };

  public type PlayerDocumentMetadata = {
    id : Nat;
    sessionId : Nat;
    owner : Principal;
    name : Text;
    createdBy : Principal;
    lastModified : Int;
    isPrivate : Bool;
  };

  public type ImageReference = {
    id : Nat;
    documentId : Nat;
    fileId : Text;
    caption : Text;
    position : Int;
    size : Int;
    createdBy : Principal;
    lastModified : Int;
    title : Text;
  };

  public type DocumentFileReference = {
    id : Nat;
    documentId : Nat;
    file : Storage.ExternalBlob;
    filename : Text;
    mimeType : Text;
    size : Nat;
    createdBy : Principal;
    lastModified : Int;
  };

  public type DiceRollResult = {
    pattern : Text;
    rolls : [Nat];
    total : Int;
    modifier : Int;
  };

  public type TurnOrder = {
    sessionId : Nat;
    order : [Text];
    currentIndex : Nat;
  };

  public type SessionCreateRequest = {
    name : Text;
    password : ?Text;
    hostNickname : Text;
  };

  public type JoinSessionRequest = {
    sessionId : Nat;
    nickname : Text;
    password : ?Text;
  };

  public type StandardResponse = {
    #ok : Text;
    #error : Text;
  };

  public type UploadFileRequest = {
    documentId : Nat;
    file : Storage.ExternalBlob;
    filename : Text;
    mimeType : Text;
    size : Nat;
  };

  public type UploadDocumentFileResponse = {
    #ok : Nat;
    #error : Text;
  };

  public type CreateDocumentResponse = {
    #ok : Nat;
    #error : Text;
  };

  public type CreatePlayerDocumentResponse = {
    #ok : Nat;
    #error : Text;
  };

  public type CreateImageResponse = {
    #ok : Nat;
    #error : Text;
  };

  public type AddImageToDocumentResponse = {
    #ok : Nat;
    #error : Text;
  };

  public type SessionExport = {
    session : Session;
    channels : [Channel];
    messages : [Message];
    documents : [Document];
    playerDocuments : [PlayerDocument];
    images : [ImageReference];
    documentFiles : [DocumentFileReference];
    turnOrder : ?TurnOrder;
  };

  public type DocumentMetadata = {
    #session : Document;
    #player : PlayerDocument;
  };

  var nextSessionId : Nat = 1;
  var nextChannelId : Nat = 1;
  var nextMessageId : Nat = 1;
  var nextDocumentId : Nat = 1;
  var nextImageId : Nat = 1;
  var nextFileId : Nat = 1;
  var nextCommentId : Nat = 1;
  var nextStickerId : Nat = 1;

  let sessions = Map.empty<Nat, Session>();
  let messages = Map.empty<Nat, List.List<Message>>();
  let documents = Map.empty<Nat, Document>();
  let playerDocumentsMap = Map.empty<Nat, PlayerDocument>();
  let turnOrders = Map.empty<Nat, TurnOrder>();
  let userProfiles = Map.empty<Principal, UserProfile>();
  let imageReferences = Map.empty<Nat, ImageReference>();
  let documentFileReferences = Map.empty<Nat, DocumentFileReference>();
  let comments = Map.empty<Nat, DocumentComment>();
  let stickers = Map.empty<Nat, Sticker>();

  func updateSessionActivity(sessionId : Nat) {
    switch (sessions.get(sessionId)) {
      case (null) {};
      case (?session) {
        let updated = {
          session with
          lastActive = Time.now();
        };
        sessions.add(sessionId, updated);
      };
    };
  };

  func hashPassword(password : Text) : Blob {
    let salt = "rpg_session_salt";
    (password # salt).encodeUtf8();
  };

  func verifyPassword(password : Text, hash : Blob) : Bool {
    let computed = hashPassword(password);
    Blob.equal(computed, hash);
  };

  func isSessionHost(sessionId : Nat, caller : Principal) : Bool {
    switch (sessions.get(sessionId)) {
      case (null) { false };
      case (?session) { Principal.equal(session.host, caller) };
    };
  };

  func isSessionMember(sessionId : Nat, caller : Principal) : Bool {
    switch (sessions.get(sessionId)) {
      case (null) { false };
      case (?session) {
        session.members.find(
          func(m) { Principal.equal(m.id, caller) }
        ) != null;
      };
    };
  };

  func getMemberNickname(sessionId : Nat, caller : Principal) : ?Text {
    switch (sessions.get(sessionId)) {
      case (null) { null };
      case (?session) {
        switch (session.members.find(func(m) { Principal.equal(m.id, caller) })) {
          case (null) { null };
          case (?member) { ?member.nickname };
        };
      };
    };
  };

  func canAccessPlayerDocument(doc : PlayerDocument, caller : Principal) : Bool {
    if (Principal.equal(doc.owner, caller)) {
      return true;
    };
    if (doc.isPrivate) {
      return isSessionHost(doc.sessionId, caller);
    };
    isSessionMember(doc.sessionId, caller);
  };

  func getSessionIdForChannel(channelId : Nat) : ?Nat {
    for ((sessionId, session) in sessions.entries()) {
      let hasChannel = session.channels.find(func(ch) { ch.id == channelId }) != null;
      if (hasChannel) {
        return ?sessionId;
      };
      let hasMembersChannel = session.membersChannels.find(func(ch) { ch.id == channelId }) != null;
      if (hasMembersChannel) {
        return ?sessionId;
      };
    };
    null;
  };

  let accessControlState = AccessControl.initState();
  include MixinAuthorization(accessControlState);

  public query ({ caller }) func getCallerUserProfile() : async ?UserProfile {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can access profiles");
    };
    userProfiles.get(caller);
  };

  public query ({ caller }) func getUserProfile(user : Principal) : async ?UserProfile {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view profiles");
    };
    userProfiles.get(user);
  };

  public shared ({ caller }) func saveCallerUserProfile(profile : UserProfile) : async () {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can save profiles");
    };
    userProfiles.add(caller, profile);
  };

  public shared ({ caller }) func removeProfilePicture() : async () {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can remove profile pictures");
    };

    switch (userProfiles.get(caller)) {
      case (null) {};
      case (?profile) {
        let newProfile = { profile with profilePicture = null };
        userProfiles.add(caller, newProfile);
      };
    };
  };

  public shared ({ caller }) func createSession(request : SessionCreateRequest) : async Session {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can create sessions");
    };

    let passwordHash = switch (request.password) {
      case (null) { null };
      case (?pwd) { ?hashPassword(pwd) };
    };

    let defaultChannel : Channel = {
      id = nextChannelId;
      name = "Main";
      createdBy = caller;
    };
    nextChannelId += 1;

    let hostMember : SessionMember = {
      id = caller;
      nickname = request.hostNickname;
      joinedAt = Time.now();
    };

    let session : Session = {
      id = nextSessionId;
      name = request.name;
      host = caller;
      passwordHash;
      members = [hostMember];
      channels = [defaultChannel];
      membersChannels = [];
      createdAt = Time.now();
      lastActive = Time.now();
    };

    sessions.add(nextSessionId, session);
    messages.add(nextSessionId, List.empty<Message>());

    nextSessionId += 1;
    session;
  };

  public shared ({ caller }) func joinSession(request : JoinSessionRequest) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      return #error("Unauthorized: Only users can join sessions");
    };

    switch (sessions.get(request.sessionId)) {
      case (null) { #error("Session not found") };
      case (?session) {
        if (isSessionMember(request.sessionId, caller)) {
          return #error("Already a member of this session");
        };

        switch (session.passwordHash) {
          case (?hash) {
            switch (request.password) {
              case (null) { return #error("Password required") };
              case (?pwd) {
                if (not verifyPassword(pwd, hash)) {
                  return #error("Invalid password");
                };
              };
            };
          };
          case (null) {};
        };

        let newMember : SessionMember = {
          id = caller;
          nickname = request.nickname;
          joinedAt = Time.now();
        };

        let updatedMembers = session.members.concat([newMember]);
        let updatedSession = {
          session with
          members = updatedMembers;
          lastActive = Time.now();
        };

        sessions.add(request.sessionId, updatedSession);
        #ok("Joined session successfully");
      };
    };
  };

  public query ({ caller }) func getSession(sessionId : Nat) : async ?Session {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view sessions");
    };

    if (not isSessionMember(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only session members can view session details");
    };
    sessions.get(sessionId);
  };

  public query ({ caller }) func listSessions() : async [Session] {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can list sessions");
    };

    sessions.values().toArray();
  };

  public shared ({ caller }) func createChannel(sessionId : Nat, name : Text) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can create channels");
    };

    if (not isSessionHost(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only the session host can create channels");
    };

    switch (sessions.get(sessionId)) {
      case (null) { #error("Session not found") };
      case (?session) {
        let newChannel : Channel = {
          id = nextChannelId;
          name;
          createdBy = caller;
        };
        nextChannelId += 1;

        let updatedChannels = session.channels.concat([newChannel]);
        let updatedSession = {
          session with
          channels = updatedChannels;
        };

        sessions.add(sessionId, updatedSession);
        updateSessionActivity(sessionId);
        #ok("Channel created");
      };
    };
  };

  public shared ({ caller }) func renameChannel(sessionId : Nat, channelId : Nat, newName : Text) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can rename channels");
    };

    if (not isSessionHost(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only the session host can rename channels");
    };

    switch (sessions.get(sessionId)) {
      case (null) { #error("Session not found") };
      case (?session) {
        let updatedChannels = session.channels.map(
          func(ch) {
            if (ch.id == channelId) {
              { ch with name = newName };
            } else {
              ch;
            };
          }
        );

        let updatedSession = {
          session with
          channels = updatedChannels;
        };

        sessions.add(sessionId, updatedSession);
        updateSessionActivity(sessionId);
        #ok("Channel renamed");
      };
    };
  };

  public shared ({ caller }) func deleteChannel(sessionId : Nat, channelId : Nat) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can delete channels");
    };

    if (not isSessionHost(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only the session host can delete channels");
    };

    switch (sessions.get(sessionId)) {
      case (null) { #error("Session not found") };
      case (?session) {
        let updatedChannels = session.channels.filter(
          func(ch) { ch.id != channelId }
        );

        if (updatedChannels.size() == 0) {
          return #error("Cannot delete the last channel");
        };

        let updatedSession = {
          session with
          channels = updatedChannels;
        };

        sessions.add(sessionId, updatedSession);
        updateSessionActivity(sessionId);
        #ok("Channel deleted");
      };
    };
  };

  public query ({ caller }) func getChannels(sessionId : Nat) : async [Channel] {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view channels");
    };

    if (not isSessionMember(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only session members can view channels");
    };

    switch (sessions.get(sessionId)) {
      case (null) { [] };
      case (?session) { session.channels };
    };
  };

  public shared ({ caller }) func createMembersChannel(sessionId : Nat, name : Text) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can create members` channels");
    };

    if (not isSessionMember(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only session members can create members` channels");
    };

    switch (sessions.get(sessionId)) {
      case (null) { #error("Session not found") };
      case (?session) {
        let newChannel : MembersChannel = {
          id = nextChannelId;
          name;
          createdBy = caller;
        };
        nextChannelId += 1;

        let updatedChannels = session.membersChannels.concat([newChannel]);
        let updatedSession = {
          session with
          membersChannels = updatedChannels;
        };

        sessions.add(sessionId, updatedSession);
        updateSessionActivity(sessionId);
        #ok("Members` channel created");
      };
    };
  };

  public shared ({ caller }) func renameMembersChannel(sessionId : Nat, channelId : Nat, newName : Text) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can rename members` channels");
    };

    switch (sessions.get(sessionId)) {
      case (null) { #error("Session not found") };
      case (?session) {
        let channel = session.membersChannels.find(
          func(ch) { ch.id == channelId }
        );

        switch (channel) {
          case (null) { return #error("Channel not found") };
          case (?ch) {
            if (not Principal.equal(ch.createdBy, caller) and not isSessionHost(sessionId, caller)) {
              Runtime.trap("Unauthorized: Only the creator or session host can rename this channel");
            };

            let updatedChannels = session.membersChannels.map(
              func(c) {
                if (c.id == channelId) {
                  { c with name = newName };
                } else {
                  c;
                };
              }
            );

            let updatedSession = {
              session with
              membersChannels = updatedChannels;
            };

            sessions.add(sessionId, updatedSession);
            updateSessionActivity(sessionId);
            #ok("Members` channel renamed");
          };
        };
      };
    };
  };

  public shared ({ caller }) func deleteMembersChannel(sessionId : Nat, channelId : Nat) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can delete members` channels");
    };

    switch (sessions.get(sessionId)) {
      case (null) { #error("Session not found") };
      case (?session) {
        let channel = session.membersChannels.find(
          func(ch) { ch.id == channelId }
        );

        switch (channel) {
          case (null) { return #error("Channel not found") };
          case (?ch) {
            if (not Principal.equal(ch.createdBy, caller) and not isSessionHost(sessionId, caller)) {
              Runtime.trap("Unauthorized: Only the creator or session host can delete this channel");
            };

            let updatedChannels = session.membersChannels.filter(
              func(c) { c.id != channelId }
            );

            let updatedSession = {
              session with
              membersChannels = updatedChannels;
            };

            sessions.add(sessionId, updatedSession);
            updateSessionActivity(sessionId);
            #ok("Members` channel deleted");
          };
        };
      };
    };
  };

  public query ({ caller }) func getMembersChannels(sessionId : Nat) : async [MembersChannel] {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view members` channels");
    };
    if (not isSessionMember(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only session members can view members` channels");
    };

    switch (sessions.get(sessionId)) {
      case (null) { [] };
      case (?session) { session.membersChannels };
    };
  };

  public shared ({ caller }) func postMessage(sessionId : Nat, channelId : Nat, content : Text, image : ?Storage.ExternalBlob, gif : ?Text, replyToId : ?Nat) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can post messages");
    };

    if (not isSessionMember(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only session members can post messages");
    };

    switch (getMemberNickname(sessionId, caller)) {
      case (null) { #error("Member not found") };
      case (?nickname) {
        let message : Message = {
          id = nextMessageId;
          channelId;
          author = nickname;
          content;
          timestamp = Time.now();
          image;
          gif;
          replyToId;
        };
        nextMessageId += 1;

        switch (messages.get(sessionId)) {
          case (null) {
            messages.add(sessionId, List.singleton<Message>(message));
          };
          case (?msgList) {
            msgList.add(message);
          };
        };

        updateSessionActivity(sessionId);
        #ok("Message posted");
      };
    };
  };

  public query ({ caller }) func getMessages(sessionId : Nat, channelId : Nat) : async [Message] {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view messages");
    };

    if (not isSessionMember(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only session members can view messages");
    };

    switch (messages.get(sessionId)) {
      case (null) { [] };
      case (?msgList) {
        let filtered = msgList.filter(
          func(m) { m.channelId == channelId }
        );
        filtered.toArray();
      };
    };
  };

  public shared ({ caller }) func createDocument(sessionId : Nat, name : Text, content : Text) : async CreateDocumentResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can create documents");
    };

    if (not isSessionHost(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only the session host can create documents");
    };

    let doc : Document = {
      id = nextDocumentId;
      sessionId;
      name;
      content;
      revision = 1;
      locked = false;
      createdBy = caller;
      lastModified = Time.now();
    };
    let docId = nextDocumentId;
    nextDocumentId += 1;

    documents.add(doc.id, doc);
    updateSessionActivity(sessionId);
    #ok(docId);
  };

  public shared ({ caller }) func renameDocument(documentId : Nat, newName : Text) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can rename documents");
    };

    switch (documents.get(documentId)) {
      case (null) { #error("Document not found") };
      case (?doc) {
        if (not isSessionHost(doc.sessionId, caller)) {
          Runtime.trap("Unauthorized: Only the session host can rename documents");
        };

        let updated = {
          doc with
          name = newName;
          lastModified = Time.now();
        };
        documents.add(documentId, updated);
        updateSessionActivity(doc.sessionId);
        #ok("Document renamed");
      };
    };
  };

  public shared ({ caller }) func lockDocument(documentId : Nat) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can lock documents");
    };

    switch (documents.get(documentId)) {
      case (null) { #error("Document not found") };
      case (?doc) {
        if (not isSessionHost(doc.sessionId, caller)) {
          Runtime.trap("Unauthorized: Only the session host can lock documents");
        };

        let updated = {
          doc with
          locked = true;
          lastModified = Time.now();
        };
        documents.add(documentId, updated);
        #ok("Document locked");
      };
    };
  };

  public shared ({ caller }) func unlockDocument(documentId : Nat) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can unlock documents");
    };

    switch (documents.get(documentId)) {
      case (null) { #error("Document not found") };
      case (?doc) {
        if (not isSessionHost(doc.sessionId, caller)) {
          Runtime.trap("Unauthorized: Only the session host can unlock documents");
        };

        let updated = {
          doc with
          locked = false;
          lastModified = Time.now();
        };
        documents.add(documentId, updated);
        #ok("Document unlocked");
      };
    };
  };

  public shared ({ caller }) func deleteDocument(documentId : Nat) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can delete documents");
    };

    switch (documents.get(documentId)) {
      case (null) { #error("Document not found") };
      case (?doc) {
        if (not isSessionHost(doc.sessionId, caller)) {
          Runtime.trap("Unauthorized: Only the session host can delete documents");
        };

        documents.remove(documentId);
        updateSessionActivity(doc.sessionId);
        #ok("Document deleted");
      };
    };
  };

  public shared ({ caller }) func editDocument(documentId : Nat, newContent : Text) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can edit documents");
    };

    switch (documents.get(documentId)) {
      case (null) { #error("Document not found") };
      case (?doc) {
        if (not isSessionMember(doc.sessionId, caller)) {
          Runtime.trap("Unauthorized: Only session members can edit documents");
        };

        if (doc.locked) {
          return #error("Document is locked");
        };

        let updated = {
          doc with
          content = newContent;
          revision = doc.revision + 1;
          lastModified = Time.now();
        };
        documents.add(documentId, updated);
        updateSessionActivity(doc.sessionId);
        #ok("Document updated to revision " # updated.revision.toText());
      };
    };
  };

  public query ({ caller }) func getDocument(documentId : Nat) : async ?Document {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view documents");
    };

    switch (documents.get(documentId)) {
      case (null) { null };
      case (?doc) {
        if (not isSessionMember(doc.sessionId, caller)) {
          Runtime.trap("Unauthorized: Only session members can view documents");
        };
        ?doc;
      };
    };
  };

  public query ({ caller }) func listDocuments(sessionId : Nat) : async [Document] {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can list documents");
    };

    if (not isSessionMember(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only session members can list documents");
    };

    let filtered = documents.values().filter(
      func(doc) { doc.sessionId == sessionId }
    );
    filtered.toArray();
  };

  func canAccessDocument(documentId : Nat, caller : Principal) : Bool {
    switch (documents.get(documentId)) {
      case (?doc) {
        return isSessionMember(doc.sessionId, caller);
      };
      case (null) {
        switch (playerDocumentsMap.get(documentId)) {
          case (?playerDoc) {
            return canAccessPlayerDocument(playerDoc, caller);
          };
          case (null) {
            return false;
          };
        };
      };
    };
  };

  public query ({ caller }) func listDocumentFiles(documentId : Nat) : async [DocumentFileReference] {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can list files");
    };

    if (not canAccessDocument(documentId, caller)) {
      Runtime.trap("Unauthorized: You cannot access files for this document");
    };

    let filteredFiles = documentFileReferences.values().filter(
      func(file) { file.documentId == documentId }
    );
    filteredFiles.toArray();
  };

  public query ({ caller }) func getDocumentFileReference(fileId : Nat) : async ?DocumentFileReference {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view file references");
    };

    let fileRef = switch (documentFileReferences.get(fileId)) {
      case (null) { return null };
      case (?ref) { ref };
    };

    if (not canAccessDocument(fileRef.documentId, caller)) {
      Runtime.trap("Unauthorized: You cannot access this file");
    };

    ?fileRef;
  };

  public query ({ caller }) func getDocumentFileBlob(fileId : Nat) : async ?Storage.ExternalBlob {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view files");
    };

    let fileRef = switch (documentFileReferences.get(fileId)) {
      case (null) { return null };
      case (?ref) { ref };
    };

    if (not canAccessDocument(fileRef.documentId, caller)) {
      Runtime.trap("Unauthorized: You cannot access this file");
    };

    ?fileRef.file;
  };

  public shared ({ caller }) func uploadDocumentFile(request : UploadFileRequest) : async UploadDocumentFileResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      return #error("Unauthorized: Only users can upload files");
    };

    if (request.size > 10_000_000) {
      return #error("File size must not exceed 10MB");
    };

    let canUpload = switch (documents.get(request.documentId)) {
      case (?doc) {
        isSessionMember(doc.sessionId, caller);
      };
      case (null) {
        switch (playerDocumentsMap.get(request.documentId)) {
          case (?playerDoc) {
            Principal.equal(playerDoc.owner, caller);
          };
          case (null) {
            return #error("Document not found for file upload");
          };
        };
      };
    };

    if (not canUpload) {
      return #error("Unauthorized: You cannot upload files to this document");
    };

    let newFileReference : DocumentFileReference = {
      id = nextFileId;
      documentId = request.documentId;
      file = request.file;
      filename = request.filename;
      mimeType = request.mimeType;
      size = request.size;
      createdBy = caller;
      lastModified = Time.now();
    };
    let fileId = nextFileId;
    nextFileId += 1;

    documentFileReferences.add(newFileReference.id, newFileReference);
    #ok(fileId);
  };

  func parseDicePattern(pattern : Text) : ?(Nat, Nat, Int) {
    let trimmed = pattern.trim(#text " ");

    var numDice : Nat = 1;
    var diceSize : Nat = 0;
    var modifier : Int = 0;

    let chars = trimmed.chars();
    var buffer = "";
    var stage = 0;

    for (c in chars) {
      if (c == 'd' or c == 'D') {
        if (buffer.size() > 0) {
          switch (Int.fromText(buffer)) {
            case (?n) { numDice := Int.abs(n) };
            case (null) {};
          };
        };
        buffer := "";
        stage := 1;
      } else if (c == '+' or c == '-') {
        if (stage == 1 and buffer.size() > 0) {
          switch (Int.fromText(buffer)) {
            case (?n) { diceSize := Int.abs(n) };
            case (null) {};
          };
        };
        buffer := Text.fromChar(c);
        stage := 2;
      } else {
        buffer #= Text.fromChar(c);
      };
    };

    if (stage == 1 and buffer.size() > 0) {
      switch (Int.fromText(buffer)) {
        case (?n) { diceSize := Int.abs(n) };
        case (null) {};
      };
    } else if (stage == 2 and buffer.size() > 0) {
      switch (Int.fromText(buffer)) {
        case (?n) { modifier := n };
        case (null) {};
      };
    };

    if (diceSize > 0 and numDice > 0 and numDice <= 100) {
      ?(numDice, diceSize, modifier);
    } else {
      null;
    };
  };

  func rollDice(numDice : Nat, diceSize : Nat) : [Nat] {
    let seed = Int.abs(Time.now());
    Array.tabulate<Nat>(
      numDice,
      func(i) {
        let roll = (seed + i * 7919) % diceSize + 1;
        Int.abs(roll);
      },
    );
  };

  public shared ({ caller }) func roll(sessionId : Nat, pattern : Text) : async DiceRollResult {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can roll dice");
    };

    if (not isSessionMember(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only session members can roll dice");
    };

    switch (parseDicePattern(pattern)) {
      case (null) {
        Runtime.trap("Invalid dice pattern. Use format like 'd20', '2d6+3', etc.");
      };
      case (?(numDice, diceSize, modifier)) {
        let rolls = rollDice(numDice, diceSize);
        var sum = 0;
        for (roll in rolls.vals()) {
          sum += roll;
        };
        let total = sum + modifier;

        updateSessionActivity(sessionId);

        {
          pattern;
          rolls;
          total;
          modifier;
        };
      };
    };
  };

  public shared ({ caller }) func setTurnOrder(sessionId : Nat, order : [Text]) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can set turn order");
    };

    if (not isSessionHost(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only the session host can set turn order");
    };

    let turnOrder : TurnOrder = {
      sessionId;
      order;
      currentIndex = 0;
    };

    turnOrders.add(sessionId, turnOrder);
    updateSessionActivity(sessionId);
    #ok("Turn order set");
  };

  public shared ({ caller }) func nextTurn(sessionId : Nat) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can advance turns");
    };

    if (not isSessionHost(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only the session host can advance turns");
    };

    switch (turnOrders.get(sessionId)) {
      case (null) { #error("No turn order set") };
      case (?turnOrder) {
        let nextIndex = (turnOrder.currentIndex + 1) % turnOrder.order.size();
        let updated = {
          turnOrder with
          currentIndex = nextIndex;
        };
        turnOrders.add(sessionId, updated);
        updateSessionActivity(sessionId);
        #ok("Advanced to next turn");
      };
    };
  };

  public shared ({ caller }) func addImageToDocument(sessionId : Nat, documentId : Nat, fileId : Text, title : Text, caption : Text, position : Int, size : Int) : async AddImageToDocumentResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can add images");
    };

    switch (documents.get(documentId)) {
      case (null) { #error("Document not found") };
      case (?doc) {
        if (doc.sessionId != sessionId) {
          Runtime.trap("Unauthorized: Document does not belong to this session");
        };

        if (not isSessionMember(sessionId, caller)) {
          Runtime.trap("Unauthorized: Only session members can add images");
        };

        if (doc.locked) {
          return #error("Cannot add images to a locked document");
        };

        let image : ImageReference = {
          id = nextImageId;
          documentId;
          fileId;
          caption;
          position;
          size;
          createdBy = caller;
          lastModified = Time.now();
          title;
        };
        let imageId = nextImageId;
        nextImageId += 1;

        imageReferences.add(image.id, image);
        #ok(imageId);
      };
    };
  };

  public query ({ caller }) func getImageReferences(documentId : Nat) : async [ImageReference] {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view images");
    };

    switch (documents.get(documentId)) {
      case (null) { [] };
      case (?doc) {
        if (not isSessionMember(doc.sessionId, caller)) {
          Runtime.trap("Unauthorized: Only session members can view images");
        };

        let filtered = imageReferences.values().filter(
          func(img) { img.documentId == documentId }
        );
        filtered.toArray();
      };
    };
  };

  public query ({ caller }) func getDocumentWithImages(documentId : Nat) : async ?DocumentWithImages {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view documents");
    };

    switch (documents.get(documentId)) {
      case (null) { null };
      case (?doc) {
        if (not isSessionMember(doc.sessionId, caller)) {
          Runtime.trap("Unauthorized: Only session members can view documents");
        };

        let images = imageReferences.values().filter(
          func(img) { img.documentId == documentId }
        );
        ?{
          doc with
          images = images.toArray();
        };
      };
    };
  };

  public shared ({ caller }) func createPlayerDocument(sessionId : Nat, name : Text, content : Text, isPrivate : Bool) : async CreatePlayerDocumentResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can create documents");
    };

    if (not isSessionMember(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only session members can create player documents");
    };

    let doc : PlayerDocument = {
      id = nextDocumentId;
      sessionId;
      owner = caller;
      name;
      content;
      createdBy = caller;
      lastModified = Time.now();
      images = [];
      isPrivate;
    };
    let docId = nextDocumentId;
    nextDocumentId += 1;

    playerDocumentsMap.add(doc.id, doc);
    updateSessionActivity(sessionId);
    #ok(docId);
  };

  public shared ({ caller }) func renamePlayerDocument(documentId : Nat, newName : Text) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can rename documents");
    };

    switch (playerDocumentsMap.get(documentId)) {
      case (null) { #error("Document not found") };
      case (?doc) {
        if (not Principal.equal(doc.owner, caller)) {
          Runtime.trap("Unauthorized: Only the owner can rename this document");
        };

        let updated = {
          doc with
          name = newName;
          lastModified = Time.now();
        };
        playerDocumentsMap.add(documentId, updated);
        updateSessionActivity(doc.sessionId);
        #ok("Player document renamed");
      };
    };
  };

  public shared ({ caller }) func editPlayerDocument(documentId : Nat, newContent : Text) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can edit documents");
    };

    switch (playerDocumentsMap.get(documentId)) {
      case (null) { #error("Document not found") };
      case (?doc) {
        if (not Principal.equal(doc.owner, caller)) {
          Runtime.trap("Unauthorized: Only the owner can edit this document");
        };

        let updated = {
          doc with
          content = newContent;
          lastModified = Time.now();
        };
        playerDocumentsMap.add(documentId, updated);
        updateSessionActivity(doc.sessionId);
        #ok("Player document updated");
      };
    };
  };

  public shared ({ caller }) func deletePlayerDocument(documentId : Nat) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can delete documents");
    };

    switch (playerDocumentsMap.get(documentId)) {
      case (null) { #error("Document not found") };
      case (?doc) {
        if (not Principal.equal(doc.owner, caller)) {
          Runtime.trap("Unauthorized: Only the owner can delete this document");
        };

        playerDocumentsMap.remove(documentId);
        updateSessionActivity(doc.sessionId);
        #ok("Player document deleted");
      };
    };
  };

  public shared ({ caller }) func setPlayerDocumentVisibility(documentId : Nat, isPrivate : Bool) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can change visibility");
    };

    switch (playerDocumentsMap.get(documentId)) {
      case (null) { #error("Document not found") };
      case (?doc) {
        if (not Principal.equal(doc.owner, caller)) {
          Runtime.trap("Unauthorized: Only the owner can change visibility");
        };

        let updated = {
          doc with
          isPrivate;
          lastModified = Time.now();
        };
        playerDocumentsMap.add(documentId, updated);
        #ok("Visibility updated");
      };
    };
  };

  public shared ({ caller }) func addImageToPlayerDocument(documentId : Nat, fileId : Text, title : Text, caption : Text, position : Int, size : Int) : async AddImageToDocumentResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can add images");
    };

    switch (playerDocumentsMap.get(documentId)) {
      case (null) { #error("Document not found") };
      case (?doc) {
        if (not Principal.equal(doc.owner, caller)) {
          Runtime.trap("Unauthorized: Only the owner can add images to this document");
        };

        let image : ImageReference = {
          id = nextImageId;
          documentId;
          fileId;
          caption;
          position;
          size;
          createdBy = caller;
          lastModified = Time.now();
          title;
        };
        let imageId = nextImageId;
        nextImageId += 1;

        imageReferences.add(image.id, image);

        let updatedImages = doc.images.concat([image]);
        let updatedDoc = {
          doc with
          images = updatedImages;
          lastModified = Time.now();
        };
        playerDocumentsMap.add(documentId, updatedDoc);

        #ok(imageId);
      };
    };
  };

  public query ({ caller }) func getPlayerDocument(documentId : Nat) : async ?PlayerDocument {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view player documents");
    };

    switch (playerDocumentsMap.get(documentId)) {
      case (null) { null };
      case (?doc) {
        if (Principal.equal(doc.owner, caller)) {
          return ?doc;
        };

        if (doc.isPrivate) {
          if (isSessionHost(doc.sessionId, caller)) {
            return ?doc;
          };
          return null;
        };

        if (not isSessionMember(doc.sessionId, caller)) {
          Runtime.trap("Unauthorized: Only session members can view player documents");
        };

        ?doc;
      };
    };
  };

  public query ({ caller }) func listPlayerDocuments(sessionId : Nat) : async [PlayerDocument] {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can list player documents");
    };

    if (not isSessionMember(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only session members can list player documents");
    };

    let isHost = isSessionHost(sessionId, caller);

    playerDocumentsMap.values().filter(func(doc) {
      if (doc.sessionId != sessionId) {
        return false;
      };
      if (Principal.equal(doc.owner, caller)) {
        return true;
      };
      if (doc.isPrivate) {
        return isHost;
      };
      true
    }).toArray();
  };

  public query ({ caller }) func listPlayerDocumentsMetadata(sessionId : Nat) : async [PlayerDocumentMetadata] {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view document metadata");
    };

    if (not isSessionHost(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only the session host can view all document metadata");
    };

    let filtered = playerDocumentsMap.values().filter(
      func(doc) { doc.sessionId == sessionId }
    );

    filtered.map(
      func(doc) : PlayerDocumentMetadata {
        {
          id = doc.id;
          sessionId = doc.sessionId;
          owner = doc.owner;
          name = doc.name;
          createdBy = doc.createdBy;
          lastModified = doc.lastModified;
          isPrivate = doc.isPrivate;
        };
      }
    ).toArray();
  };

  public query ({ caller }) func exportSession(sessionId : Nat) : async ?SessionExport {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can export sessions");
    };

    if (not isSessionHost(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only the session host can export sessions");
    };

    switch (sessions.get(sessionId)) {
      case (null) { null };
      case (?session) {
        let sessionMessages = switch (messages.get(sessionId)) {
          case (null) { [] };
          case (?msgList) { msgList.toArray() };
        };

        let sessionDocuments = documents.values().filter(
          func(doc) { doc.sessionId == sessionId }
        ).toArray();

        let sessionPlayerDocuments = playerDocumentsMap.values().filter(
          func(doc) { doc.sessionId == sessionId }
        ).toArray();

        let sessionImages = imageReferences.values().toArray();

        let sessionDocumentFiles = documentFileReferences.values().toArray();

        let sessionTurnOrder = turnOrders.get(sessionId);

        ?{
          session;
          channels = session.channels;
          messages = sessionMessages;
          documents = sessionDocuments;
          playerDocuments = sessionPlayerDocuments;
          images = sessionImages;
          documentFiles = sessionDocumentFiles;
          turnOrder = sessionTurnOrder;
        };
      };
    };
  };

  public shared ({ caller }) func importSession(exportData : SessionExport) : async StandardResponse {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can import sessions");
    };

    if (not Principal.equal(exportData.session.host, caller)) {
      Runtime.trap("Unauthorized: Only the original host can import this session");
    };

    let newSessionId = nextSessionId;
    nextSessionId += 1;

    let importedSession = {
      exportData.session with
      id = newSessionId;
      lastActive = Time.now();
    };
    sessions.add(newSessionId, importedSession);

    let msgList = List.empty<Message>();
    for (msg in exportData.messages.vals()) {
      let newMsg = {
        msg with
        id = nextMessageId;
      };
      nextMessageId += 1;
      msgList.add(newMsg);
    };
    messages.add(newSessionId, msgList);

    for (doc in exportData.documents.vals()) {
      let newDoc = {
        doc with
        id = nextDocumentId;
        sessionId = newSessionId;
      };
      nextDocumentId += 1;
      documents.add(newDoc.id, newDoc);
    };

    for (doc in exportData.playerDocuments.vals()) {
      let newDoc = {
        doc with
        id = nextDocumentId;
        sessionId = newSessionId;
      };
      nextDocumentId += 1;
      playerDocumentsMap.add(newDoc.id, newDoc);
    };

    for (img in exportData.images.vals()) {
      let newImg = {
        img with
        id = nextImageId;
      };
      nextImageId += 1;
      imageReferences.add(newImg.id, newImg);
    };

    for (file in exportData.documentFiles.vals()) {
      let newFile = {
        file with
        id = nextFileId;
      };
      nextFileId += 1;
      documentFileReferences.add(newFile.id, newFile);
    };

    switch (exportData.turnOrder) {
      case (null) {};
      case (?turnOrder) {
        let newTurnOrder = {
          turnOrder with
          sessionId = newSessionId;
        };
        turnOrders.add(newSessionId, newTurnOrder);
      };
    };

    #ok("Session imported with ID: " # newSessionId.toText());
  };

  public query ({ caller }) func getImages(sessionId : Nat) : async [ImageReference] {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view images");
    };

    if (not isSessionMember(sessionId, caller)) {
      Runtime.trap("Unauthorized: Only session members can view images");
    };

    let sessionDocIds = documents.values().filter(
      func(doc) { doc.sessionId == sessionId }
    ).map(func(doc) { doc.id });

    let filtered = imageReferences.values().filter(
      func(img) {
        sessionDocIds.find(func(docId) { docId == img.documentId }) != null;
      }
    );

    filtered.toArray();
  };

  public shared ({ caller }) func addComment(documentId : Nat, text : Text) : async Nat {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can add comments");
    };

    // Check if it's a session document
    switch (documents.get(documentId)) {
      case (?doc) {
        if (not isSessionMember(doc.sessionId, caller)) {
          Runtime.trap("Unauthorized: Only session members can comment on session documents");
        };
      };
      case (null) {
        // Check if it's a player document
        switch (playerDocumentsMap.get(documentId)) {
          case (?playerDoc) {
            if (not canAccessPlayerDocument(playerDoc, caller)) {
              Runtime.trap("Unauthorized: You cannot comment on this player document");
            };
          };
          case (null) {
            Runtime.trap("Document not found");
          };
        };
      };
    };

    let newComment : DocumentComment = {
      id = nextCommentId;
      documentId;
      author = caller;
      text;
      timestamp = Time.now();
    };

    comments.add(nextCommentId, newComment);

    let commentId = nextCommentId;
    nextCommentId += 1;
    commentId;
  };

  public query ({ caller }) func getComments(documentId : Nat) : async [DocumentComment] {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view comments");
    };

    // Check if it's a session document
    switch (documents.get(documentId)) {
      case (?doc) {
        if (not isSessionMember(doc.sessionId, caller)) {
          Runtime.trap("Unauthorized: Only session members can view comments on session documents");
        };
      };
      case (null) {
        // Check if it's a player document
        switch (playerDocumentsMap.get(documentId)) {
          case (?playerDoc) {
            if (not canAccessPlayerDocument(playerDoc, caller)) {
              Runtime.trap("Unauthorized: You cannot view comments on this player document");
            };
          };
          case (null) {
            Runtime.trap("Document not found");
          };
        };
      };
    };

    (comments.values().toArray()).filter(func(comment) { comment.documentId == documentId });
  };

  public shared ({ caller }) func deleteComment(commentId : Nat) : async () {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can delete comments");
    };

    let oldComment = switch (comments.get(commentId)) {
      case (null) { Runtime.trap("Comment not found") };
      case (?comment) { comment };
    };

    if (not Principal.equal(oldComment.author, caller)) {
      Runtime.trap("Unauthorized: You can only delete your own comments");
    };

    comments.remove(commentId);
  };

  public shared ({ caller }) func updateComment(commentId : Nat, text : Text) : async () {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can update comments");
    };

    let oldComment = switch (comments.get(commentId)) {
      case (null) { Runtime.trap("Comment not found") };
      case (?comment) { comment };
    };

    if (not Principal.equal(oldComment.author, caller)) {
      Runtime.trap("Unauthorized: You can only update your own comments");
    };

    let newComment : DocumentComment = {
      oldComment with
      text;
      timestamp = Time.now();
    };

    comments.add(commentId, newComment);
  };

  // ------------------- Sticker API -------------------

  public shared ({ caller }) func addSticker(image : Storage.ExternalBlob, name : Text) : async ?Nat {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can add stickers");
    };

    let newSticker : Sticker = {
      id = nextStickerId;
      image;
      name;
      messageId = null;
      sender = null;
      channelId = null;
      timestamp = null;
    };

    stickers.add(nextStickerId, newSticker);
    let stickerId = nextStickerId;
    nextStickerId += 1;
    ?stickerId;
  };

  public shared ({ caller }) func sendSticker(stickerId : Nat, channelId : Nat, sender : Text, messageId : Nat, timestamp : Int) : async Bool {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can send stickers");
    };

    switch (getSessionIdForChannel(channelId)) {
      case (null) {
        Runtime.trap("Channel not found");
      };
      case (?sessionId) {
        if (not isSessionMember(sessionId, caller)) {
          Runtime.trap("Unauthorized: Only session members can send stickers to this channel");
        };

        switch (stickers.get(stickerId)) {
          case (null) { false };
          case (?sticker) {
            let updatedSticker = {
              sticker with
              messageId = ?messageId;
              sender = ?sender;
              channelId = ?channelId;
              timestamp = ?timestamp;
            };

            stickers.add(stickerId, updatedSticker);
            updateSessionActivity(sessionId);
            true;
          };
        };
      };
    };
  };

  public query ({ caller }) func getSticker(stickerId : Nat) : async ?Sticker {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view stickers");
    };

    stickers.get(stickerId);
  };

  public query ({ caller }) func getAllStickers() : async [Sticker] {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view stickers");
    };

    stickers.values().toArray();
  };

  public query ({ caller }) func getStickersByChannel(channelId : Nat) : async [Sticker] {
    if (not (AccessControl.hasPermission(accessControlState, caller, #user))) {
      Runtime.trap("Unauthorized: Only users can view stickers");
    };

    switch (getSessionIdForChannel(channelId)) {
      case (null) {
        Runtime.trap("Channel not found");
      };
      case (?sessionId) {
        if (not isSessionMember(sessionId, caller)) {
          Runtime.trap("Unauthorized: Only session members can view stickers for this channel");
        };

        let allStickers = stickers.values().toArray();
        allStickers.filter(
          func(sticker) {
            switch (sticker.channelId) {
              case (null) { false };
              case (?id) { id == channelId };
            };
          }
        );
      };
    };
  };
};
