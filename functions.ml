open Ast
module StringMap = Map.Make(String)
let builtin_fns = 
    let add_func map (typ, name, (formallist : bind list)) = StringMap.add name {
      rtyp = typ; 
      fname = name; 
      formals = formallist;
      body = [] } map in List.fold_left add_func StringMap.empty [ 

        (* node functions *)
        (Int, "getNodeValue", [Node, "n"]);
        (Void, "setNodeValue", [(Node, "n");(Int,"val")]);
        (Node, "makeNode", [(String, "n");(Int,"val")]);
        (String,"getNodeLabel", [Node, "n"]);

        (* Edge Functions *)                  
        (Edge, "makeEdge", [(Node, "s");(Node, "d");(Int, "val");(Bool, "directed")]);
        (Void, "setEdgeValue", [(Edge, "e");(Int,"val")]);
        (Int, "getEdgeValue", [Edge, "e"]);   

        (* NList Functions *)
        (NList, "createNList",[]);
        (Int,"nlistLen",[(NList,"n_list")]);
        (Void,"appendNode",[(NList,"g");(Node,"n")]);
        (Bool,"findNode",[(NList,"n");(Node,"node")]);
        (Void,"deleteNode",[(NList,"n");(Node,"node")]);

         (* EList Functions *)
        (EList, "createEList",[]);
        (Int,"elistLen",[(EList,"e_list")]);
        (Void,"appendEdge",[(EList,"g");(Edge,"e")]);
        (Bool,"findEdge",[(EList,"n");(Edge,"edge")]);
        (Void,"deleteEdge",[(EList,"e");(Edge,"edge")]);

        (* Graph Functions *)
        (Graph,"makeGraph",[( NList,"n");(EList,"e")]);
        (Node,"getNodeFromGraph",[(Graph,"g");(String,"name")]);
        (Edge,"getEdgeFromGraph",[(Graph,"g");(Node,"a");(Node,"b")]);
        (NList, "getAllNodes", [(Graph, "g")]);
        (EList, "getAllEdges", [(Graph, "g")]);
        (Void,"addEdge",[(Graph,"g");(Edge,"e")]);
        (Void,"addNode",[(Graph,"g");(Node,"n")]);
        (Void,"removeEdge",[(Graph,"g");(Edge,"e")]);
        (Void,"removeNode",[(Graph,"g");(Node,"n")]);
        (NList,"getNeighbours",[(Graph,"g");(Node,"n")]);
        (Void,"updateNode",[(Graph,"g");(Node,"n");(Int,"val")]);
        (Bool,"search",[(Graph,"g");(String,"val");(String,"search_options")]);
        (Bool,"findPath",[(Graph,"g");(Node,"src");(Node,"dest")]);
        (Void,"djikstras",[(Graph,"g");(Node,"src");(Node,"dest")]);
      ]
    
