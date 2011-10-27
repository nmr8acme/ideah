package ideah.formatter;

import com.intellij.formatting.*;
import com.intellij.openapi.util.TextRange;
import org.jetbrains.annotations.NotNull;

import java.util.Collections;
import java.util.List;

final class FakeBlock implements Block {

    private final TextRange range;

    FakeBlock(TextRange range) {
        this.range = range;
    }

    @NotNull
    public TextRange getTextRange() {
        return range;
    }

    @NotNull
    public List<Block> getSubBlocks() {
        return Collections.emptyList();
    }

    public Wrap getWrap() {
        return null;
    }

    public Indent getIndent() {
        return null;
    }

    public Alignment getAlignment() {
        return null;
    }

    public Spacing getSpacing(Block child1, Block child2) {
        return null;
    }

    @NotNull
    public ChildAttributes getChildAttributes(int newChildIndex) {
        return new ChildAttributes(null, null);
    }

    public boolean isIncomplete() {
        return false;
    }

    public boolean isLeaf() {
        return true;
    }
}
