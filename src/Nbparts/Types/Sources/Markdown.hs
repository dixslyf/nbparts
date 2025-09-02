module Nbparts.Types.Sources.Markdown where

import Commonmark
  ( Format,
    HasAttributes,
    IsBlock
      ( blockQuote,
        codeBlock,
        heading,
        list,
        paragraph,
        plain,
        rawBlock,
        referenceLinkDefinition,
        thematicBreak
      ),
    IsInline
      ( code,
        emph,
        entity,
        escapedChar,
        image,
        lineBreak,
        link,
        rawInline,
        softBreak,
        str,
        strong
      ),
    ListSpacing,
    ListType,
    Rangeable (ranged),
    SourceRange,
  )
import Commonmark.Types (HasAttributes (addAttributes))
import Data.Data (Data, Typeable)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)

newtype Inlines = Inlines (Seq Inline)
  deriving (Show, Semigroup, Monoid, Data, Typeable)

data Inline
  = Inline
  { inlineType :: InlineType,
    sourceRange :: SourceRange,
    attributes :: Map Text Text
  }
  deriving (Show, Data, Typeable)

data InlineType
  = Code Text
  | Emph Inlines
  | Entity Text
  | EscapedChar Char
  | Image Text Text Inlines
  | LineBreak
  | Link Text Text Inlines
  | RawInline Format Text
  | SoftBreak
  | Str Text
  | Strong Inlines
  deriving (Show, Data, Typeable)

singletonInlines :: InlineType -> Inlines
singletonInlines = Inlines . Seq.singleton . mkInline

mkInline :: InlineType -> Inline
mkInline ilType = Inline ilType mempty mempty

instance Rangeable Inlines where
  ranged srcRange' (Inlines ils) =
    Inlines $
      fmap
        (\(Inline ilType srcRange attrs) -> Inline ilType (srcRange <> srcRange') attrs)
        ils

instance HasAttributes Inlines where
  addAttributes attrs (Inlines ils) =
    Inlines $
      fmap
        (\(Inline ilType srcRange ilAttrs) -> Inline ilType srcRange $ ilAttrs <> Map.fromList attrs)
        ils

instance IsInline Inlines where
  lineBreak = singletonInlines LineBreak
  softBreak = singletonInlines SoftBreak
  str = singletonInlines . Str
  entity = singletonInlines . Entity
  escapedChar = singletonInlines . EscapedChar
  emph = singletonInlines . Emph
  strong = singletonInlines . Strong
  link target title ils = singletonInlines $ Link target title ils
  image target title ils = singletonInlines $ Image target title ils
  code = singletonInlines . Code
  rawInline format txt = singletonInlines $ RawInline format txt

newtype Blocks = Blocks (Seq Block)
  deriving (Show, Semigroup, Monoid, Data, Typeable)

data Block = Block
  { blockType :: BlockType,
    sourceRange :: SourceRange,
    attributes :: Map Text Text
  }
  deriving (Show, Data, Typeable)

data BlockType
  = Paragraph Inlines
  | Plain Inlines
  | ThematicBreak
  | BlockQuote Blocks
  | CodeBlock Text Text
  | Heading Int Inlines
  | RawBlock Format Text
  | ReferenceLinkDefinition Text (Text, Text)
  | List ListType ListSpacing Blocks
  deriving (Show, Data, Typeable)

singletonBlocks :: BlockType -> Blocks
singletonBlocks = Blocks . Seq.singleton . mkBlock

mkBlock :: BlockType -> Block
mkBlock blockType = Block blockType mempty mempty

instance Rangeable Blocks where
  ranged srcRange' (Blocks blks) =
    Blocks $
      fmap
        (\(Block blkType srcRange attrs) -> Block blkType (srcRange <> srcRange') attrs)
        blks

instance HasAttributes Blocks where
  addAttributes attrs (Blocks blks) =
    Blocks $
      fmap
        (\(Block blkType srcRange blkAttrs) -> Block blkType srcRange $ blkAttrs <> Map.fromList attrs)
        blks

instance IsBlock Inlines Blocks where
  paragraph = singletonBlocks . Paragraph
  plain = singletonBlocks . Plain
  thematicBreak = singletonBlocks ThematicBreak
  blockQuote = singletonBlocks . BlockQuote
  codeBlock info txt = singletonBlocks $ CodeBlock info txt
  heading level ils = singletonBlocks $ Heading level ils
  rawBlock format txt = singletonBlocks $ RawBlock format txt
  referenceLinkDefinition label (dest, title) = singletonBlocks $ ReferenceLinkDefinition label (dest, title)
  list listType listSpacing blks = singletonBlocks $ List listType listSpacing (mconcat blks)
