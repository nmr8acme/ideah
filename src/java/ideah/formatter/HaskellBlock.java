package ideah.formatter;

import com.intellij.formatting.*;
import com.intellij.openapi.util.TextRange;
import ideah.tree.Located;
import org.jetbrains.annotations.NotNull;

import java.util.List;

final class HaskellBlock implements Block {

    private final Located node;
    private final TextRange range;
    private final List<Block> subBlocks;

    HaskellBlock(Located node, TextRange range, List<Block> subBlocks) {
        this.node = node;
        this.range = range;
        this.subBlocks = subBlocks;
    }

    @NotNull
    public TextRange getTextRange() {
        return range;
    }

    @NotNull
    public List<Block> getSubBlocks() {
        return subBlocks;
    }

    public Wrap getWrap() {
        return null; // todo
    }

    public Indent getIndent() {
        return Indent.getNoneIndent(); // todo
    }

    public Alignment getAlignment() {
        return null; // todo
    }

    public Spacing getSpacing(Block child1, Block child2) {
        //return Spacing.createSpacing(1, 2, 1, true, 2); // todo
        return null;
    }

    @NotNull
    public ChildAttributes getChildAttributes(int newChildIndex) {
        return new ChildAttributes(null, null); // todo
    }

    public boolean isIncomplete() {
        return false; // todo
    }

    public boolean isLeaf() {
        return subBlocks.isEmpty();
    }

    @Override
    public String toString() {
        return node.getClass().getSimpleName() + " " + range.toString();
    }
}
