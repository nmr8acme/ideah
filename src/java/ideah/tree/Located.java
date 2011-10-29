package ideah.tree;

import com.google.common.collect.Iterables;
import com.intellij.formatting.*;
import com.intellij.openapi.util.TextRange;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public abstract class Located implements Block {

    public final IRange location;
    protected final List<Located> allChildren = new ArrayList<Located>();
    private List<Block> subBlocks = Collections.emptyList();

    protected Indent indent = Indent.getNoneIndent();

    protected Located(IRange location) {
        this.location = location;
    }

    protected abstract Iterable<? extends Located> getChildren();

    public final void buildBlocks(SortedMap<ILocation, Filler> tokens, RangeFactory factory) {
        doFillGaps(tokens, factory);
        doSorting();
        doFormat();
    }

    private void doFillGaps(SortedMap<ILocation, Filler> tokens, RangeFactory factory) {
        Iterable<? extends Located> children = getChildren();
        for (Located child : children) {
            child.doFillGaps(tokens, factory);
        }
        Filler range = tokens.get(location.getStart());
        if (range != null && range.location.equals(location)) {
            tokens.remove(location.getStart());
        }
        setGaps(children, tokens, factory);
    }

    private void setGaps(Iterable<? extends Located> children,
                         SortedMap<ILocation, Filler> rest,
                         RangeFactory factory) {
        Iterables.addAll(allChildren, children);

        SortedMap<ILocation, Filler> tail = rest.tailMap(location.getStart());
        for (Iterator<Map.Entry<ILocation, Filler>> i = tail.entrySet().iterator(); i.hasNext(); ) {
            Map.Entry<ILocation, Filler> entry = i.next();
            ILocation pos = entry.getKey();
            if (pos.compareTo(location.getEnd()) >= 0)
                break;
            allChildren.add(entry.getValue());
            i.remove();
        }

        rebuildStructure(factory);
    }

    private void doSorting() {
        for (Located child : allChildren) {
            child.doSorting();
        }
        Collections.sort(allChildren, new Comparator<Located>() {
            public int compare(Located o1, Located o2) {
                return o1.location.getStart().compareTo(o2.location.getStart());
            }
        });
        subBlocks = new ArrayList<Block>(allChildren);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + " " + location;
    }

    @NotNull
    public final TextRange getTextRange() {
        return location.getRange();
    }

    @NotNull
    public final List<Block> getSubBlocks() {
        return subBlocks;
    }

    public Wrap getWrap() {
        return null;
    }

    public Indent getIndent() {
        return indent;
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

    public final boolean isIncomplete() {
        return false;
    }

    public final boolean isLeaf() {
        return subBlocks.isEmpty();
    }

    private void doFormat() {
        format();
        for (Located child : allChildren) {
            child.doFormat();
        }
    }

    protected void format() {
    }

    protected void rebuildStructure(RangeFactory factory) {
    }

    protected boolean isKeyword(String keyword) {
        return false;
    }

    protected Located findKeyword(String keyword) {
        for (Located child : allChildren) {
            if (child.isKeyword(keyword))
                return child;
        }
        return null;
    }

    protected static IRange minMax(Iterable<? extends Located> nodes, RangeFactory factory) {
        ILocation min = null;
        ILocation max = null;
        for (Located node : nodes) {
            ILocation start = node.location.getStart();
            ILocation end = node.location.getEnd();
            if (min == null || start.compareTo(min) < 0) {
                min = start;
            }
            if (max == null || end.compareTo(max) > 0) {
                max = end;
            }
        }
        return factory.create(min, max);
    }
}
