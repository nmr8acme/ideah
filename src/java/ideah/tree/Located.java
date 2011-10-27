package ideah.tree;

import com.google.common.collect.Iterables;
import ideah.util.LineCol;
import ideah.util.LineColRange;

import java.util.*;

public abstract class Located {

    public final LineColRange location;
    private List<Filler> filler = null;

    protected Located(LineColRange location) {
        this.location = location;
    }

    protected abstract Iterable<? extends Located> getChildren();

    public final void fillGaps(SortedMap<LineCol, Filler> tokens) {
        Iterable<? extends Located> children = getChildren();
        for (Located child : children) {
            child.fillGaps(tokens);
        }
        Filler range = tokens.get(location.start);
        if (range != null && range.location.equals(location)) {
            tokens.remove(location.start);
        }
        setGaps(tokens);
    }

    private void setGaps(SortedMap<LineCol, Filler> rest) {
        SortedMap<LineCol, Filler> tail = rest.tailMap(location.start);
        for (Iterator<Map.Entry<LineCol, Filler>> i = tail.entrySet().iterator(); i.hasNext(); ) {
            Map.Entry<LineCol, Filler> entry = i.next();
            LineCol pos = entry.getKey();
            if (pos.compareTo(location.end) >= 0)
                break;
            if (filler == null) {
                filler = new ArrayList<Filler>();
            }
            filler.add(entry.getValue());
            i.remove();
        }
    }

    public final List<Located> getBlocks() {
        List<Located> list = new ArrayList<Located>();
        Iterables.addAll(list, getChildren());
        if (filler != null) {
            list.addAll(filler);
        }
        Collections.sort(list, new Comparator<Located>() {
            public int compare(Located o1, Located o2) {
                return o1.location.start.compareTo(o2.location.start);
            }
        });
        return list;
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + " " + location;
    }
}
