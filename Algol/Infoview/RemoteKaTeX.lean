
import Lean.Widget.UserWidget

open Lean Elab Command Json

@[widget_module]
def renderWidget : Widget.Module where
  javascript := "
  import * as React from 'react';
  import katex from 'https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/katex.mjs';
  document.head.insertAdjacentHTML('beforeend', '<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/katex.min.css\" integrity=\"sha384-5TcZemv2l/9On385z///+d7MSYlvIEw9FuZTIdZ14vJLqWphw7e7ZPuOiCHJcFCP\" crossorigin=\"anonymous\">');

  export default function(props) {
    const html = katex.renderToString(props.raw, {
      displayMode: props.display,
    });
    return React.createElement('div', {
      dangerouslySetInnerHTML: { __html: html }
    })
  }
  "

syntax renderWidgetSpec := term
syntax (name := renderCmd) "#show " renderWidgetSpec : command

structure RenderWidgetProps where
  raw : String
  display : Bool := false
deriving Server.RpcEncodable

def String.render (s : String)
                  (display : Bool := false)
    : RenderWidgetProps :=
  { raw := s, display := display }

structure JsonRpcStore where
  props: StateM Server.RpcObjectStore Json
deriving Server.RpcEncodable

def elabRenderWidgetSpecAux (props : Term) : TermElabM Expr := do
  Term.elabTerm (expectedType? := mkConst ``JsonRpcStore) <| ← `({ props := Server.RpcEncodable.rpcEncode $props })

def elabRenderWidgetSpec : TSyntax ``renderWidgetSpec → TermElabM Expr
  | `(renderWidgetSpec | $props:term) => do elabRenderWidgetSpecAux props
  | _ => throwUnsupportedSyntax

unsafe def evalStateRpcJsonUnsafe (e : Expr) : MetaM JsonRpcStore :=
  Lean.Meta.evalExpr' JsonRpcStore ``JsonRpcStore e

@[implemented_by evalStateRpcJsonUnsafe]
opaque evalStateRpcJson (e : Expr) : MetaM JsonRpcStore

@[command_elab renderCmd] def elabRenderCmd : CommandElab
  | stx@`(#show $s) => liftTermElabM do
    let expr : Expr ← elabRenderWidgetSpec s
    let ins ← evalStateRpcJson expr
    Widget.savePanelWidgetInfo
      renderWidget.javascriptHash ins.props stx
  | _ => throwUnsupportedSyntax
