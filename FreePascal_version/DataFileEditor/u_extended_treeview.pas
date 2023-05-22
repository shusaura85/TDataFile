unit u_extended_treeview;

{$mode objfpc}

interface

uses Windows, Messages, SysUtils, Variants, Classes, ComCtrls;

type
  TDFTreeNode = class(TTreeNode)
  public
    Section: string;
  end;

function GetPathFromNode(ANode: TTreeNode): String;
function GetNodeByText(ATree : TTreeView; AValue:String; AVisible: Boolean = false): TTreeNode;


implementation

// build a text representation of the tree structure for the selected node
function GetPathFromNode(ANode: TTreeNode): String;
begin
if ANode <> nil then
   begin
    if ANode.Parent <> nil then
        Result := GetPathFromNode(ANode.Parent) + '/' + ANode.Text
    else
        Result := ANode.Text;
   end
else
    Result := '';
end;

function GetNodeByText(ATree : TTreeView; AValue:String; AVisible: Boolean = false): TTreeNode;
var
 Node: TTreeNode;
 path: string;
begin
Result := nil;
if ATree.Items.Count = 0 then Exit;
Node := ATree.Items[0];
while Node <> nil do
     begin
     path := UpperCase(TDFTreeNode(Node).Section);
     if path = '' then path := GetPathFromNode(Node); // if the DFTreeNode.Section is missing, build it
     if path = UpperCase(AValue) then
        begin
        Result := Node;
        if AVisible then Result.MakeVisible;
        Break;
        end;
     Node := Node.GetNext;
     end;
end;


{
To use this node type in a treeview, you need to specify in the OnCreateNodeClass
event to use the node type

// --------------------------
procedure TForm1.TreeView1CreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
NodeClass := TDFTreeNode;
end;
// --------------------------

to access/read/write data to custom properties, use
TDFTreeNode(Node).

}

end.
