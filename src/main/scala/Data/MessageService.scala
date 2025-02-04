package Data

import Chat.ExprTree
import Data.MessageService.{MsgContent, Username}
import scalatags.Text.Frag


object MessageService:
    type Username = String
    type MsgContent = Frag

trait MessageService:
    /**
      * Retrieve the latest N added messages
      * @param n The number of message to retrieve
      * @return The content of the messages and their senders
      */
    def getLatestMessages(n: Int): Seq[(Username, MsgContent)]

    /**
      * Add a message to the history
      * @param sender The username of the sender
      * @param msg The content of the message
      * @param mention The name if it exists of the user mentioned at the start of the message with an '@'. For example the message "@Julian Hello" mentions "Julian"
      * @param exprType If the message is to the bot, the type of the query
      * @param replyToId If the message is a reply to another message, the id of the other message. Used for example, when the bot answers to a query
      * @return the unique id of the added message (generated by a method of your choice)
      */
    def add(sender: Username, msg: MsgContent, mention: Option[Username] = None, exprType: Option[ExprTree] = None, replyToId: Option[Long] = None): Long

    /**
      * Deletes all the stored messages
      */
    def deleteHistory(): Unit

class MessageImpl extends MessageService:
    // TODO - Part 3 Step 4a: Store the messages and the corresponding user in memory.
    //       Implement methods to add new messages, to get the last 20 messages and to delete all existing messages.

    override def add(sender: Username, msg: MsgContent, mention: Option[Username] = None, exprType: Option[ExprTree] = None, replyToId: Option[Long] = None): Long =
        ???

    override def getLatestMessages(n: Int) =
        ???

    override def deleteHistory(): Unit =
        ???
