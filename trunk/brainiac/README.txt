See http://brainiac.codeplex.com/

Using the DaxPlugin the system when defining a workspace will accept the url of a AltAIMLBot servitor.

Initial value is http://localhost:8123/


It will the list from the behavior server, retrieve individual behaviors and on export will return them to the server. These changes go to the live cache directory, and currently will be overwritten on the next complete reload.



The system will repartition a BTXML fragment to make it more tree like:
i.e. <say><sapi> I want to say <silence> something <bookmark> profound </sapi></say>

will become something like :
<say>
<sapi> I want to say</sapi>
<sapi><silence/></sapi>
<sapi>something</sapi>
<sapi><bookmark></sapi>
<sapi>profound</sapi>
</say>

Which will allow individual elements to be moved and edited.


Controls v2
To load an existing behaviour, double click it in the the node explorer (on the left).
To drag the graph around, hold down the left mouse button on the graph and move your mouse.
To zoom in and out, scroll your mouse wheel over the graph.
To attach a new node, drag it from the node explorer (on the left) on a node in the graph (on the right). Arrows will appear which allow you to add the node in a specific location of the graph.
To delete a single node from the graph, select it by left clicking it and press the delete key. The children will be added to the parent node of the deleted node. The node will not be deleted if this is not possible.
To delete a node from the graph as well as its children, select it by left clicking it and press Shift+Delete.
To move a single node in the graph, drag it on another node by holding down the right mouse button. Arrows will appear which allow you to place the node in a specific location of the graph.
To move a node as well as its children, drag it on another node by holding down Shift + RMB.
To attach an event to a node (except root behaviour, conditions and actions), drag it from the node explorer (on the left) on a node in the graph (on the right), holding down the left mouse button.
To select an event, you must first select the node it is attached to and then left click the event itself.
To edit a node or event, it must be selected.
To delete a behaviour or folder from the node explorer, you can also press the delete key.
To duplicate a single node by dragging it using Ctrl and the left mouse button.
To duplicate a node as well as its children, drag it using Ctrl + Shift + LMB.


More details to follow....
