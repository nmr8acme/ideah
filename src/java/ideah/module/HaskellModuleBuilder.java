package ideah.module;

import com.intellij.ide.util.projectWizard.JavaModuleBuilder;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkTypeId;
import com.intellij.openapi.projectRoots.impl.SdkConfigurationUtil;
import com.intellij.openapi.roots.ModifiableRootModel;
import com.intellij.openapi.roots.ProjectRootManager;
import ideah.sdk.HaskellSdkType;
import ideah.util.GHCUtil;
import ideah.util.GHCVersion;

import java.util.Comparator;

// todo: setup page?
public final class HaskellModuleBuilder extends JavaModuleBuilder {

    @Override
    public HaskellModuleType getModuleType() {
        return HaskellModuleType.INSTANCE;
    }

    @Override
    public boolean isSuitableSdkType(SdkTypeId sdkType) {
        return sdkType == HaskellSdkType.INSTANCE;
    }

    @Override
    public void setupRootModel(ModifiableRootModel rootModel) throws ConfigurationException {
        ProjectJdkTable table = ProjectJdkTable.getInstance();
        Sdk[] sdks = table.getAllJdks();
        Sdk ghc = null;
        for (Sdk sdk : sdks) {
            if (sdk.getSdkType().equals(HaskellSdkType.INSTANCE)) {
                ghc = sdk;
                break;
            }
        }
        if (ghc == null) {
            Comparator<Sdk> sdkComparator = new Comparator<Sdk>() {
                public int compare(Sdk s1, Sdk s2) {
                    GHCVersion v1 = GHCUtil.getVersion(s1.getVersionString());
                    GHCVersion v2 = GHCUtil.getVersion(s2.getVersionString());
                    return -v1.compareTo(v2);
                }
            };
            ghc = SdkConfigurationUtil.findOrCreateSdk(sdkComparator, HaskellSdkType.INSTANCE);
        }
        if (ghc != null) {
            Project project = rootModel.getProject();
            // todo: do not reset if overriden by user?
            ProjectRootManager.getInstance(project).setProjectSdk(ghc);
            setModuleJdk(ghc); // todo: inherit SDK from project?
        }
        // todo: do not use tabs in project
        super.setupRootModel(rootModel);
    }
}
