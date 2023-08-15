import Lean

open Lean Server Lsp RequestM JsonRpc

namespace AwesomeDashboards.Lsp

def spaces : Nat → String
| 0 => ""
| (n+1) => "→" ++ spaces n

partial def str (stx : Syntax) (n : Nat) : String := match stx with
| .node i k vs => s!"{spaces n}{k.toString}(\n{String.join <| Array.toList <| vs.map (fun v => str v (n+2))}\n)"
| .atom i val => s!"{spaces n}{val}\n"
| _ => s!"{spaces n}else\n"

def handleHover (p : HoverParams)
(prev : RequestTask (Option Hover)) : RequestM (RequestTask (Option Hover)) := do
  let doc ← readDoc
  let text := doc.meta.text
  let leanHoverPos := text.lspPosToUtf8Pos p.position
  -- TODO: find a way to capture the correct scope
  bindWaitFindSnap doc (fun snap => snap.endPos > leanHoverPos) (notFoundX := pure prev) fun snap => do
    
    return Task.pure $ .ok $ .some {
      contents := {
        kind := .plaintext
        value := str snap.stx 0
      }
    }

initialize
  Lean.Server.chainLspRequestHandler "textDocument/hover" _ _ AwesomeDashboards.Lsp.handleHover

end AwesomeDashboards.Lsp
