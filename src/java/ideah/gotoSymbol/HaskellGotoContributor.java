package ideah.gotoSymbol;

import com.intellij.navigation.GotoClassContributor;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.project.Project;

public final class HaskellGotoContributor implements GotoClassContributor {

    public String getQualifiedNameSeparator() {
        return "."; // todo
    }

    public String getQualifiedName(NavigationItem item) {
        return item.getName(); // todo
    }

    public String[] getNames(Project project, boolean includeNonProjectItems) {
        return new String[0]; // todo
    }

    public NavigationItem[] getItemsByName(String name, String pattern, Project project, boolean includeNonProjectItems) {
        return new NavigationItem[0]; // todo
    }
}
