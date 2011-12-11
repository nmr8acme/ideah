package ideah.sdk;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.projectRoots.AdditionalDataConfigurable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkAdditionalData;
import com.intellij.openapi.projectRoots.SdkModificator;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.vfs.VirtualFile;
import ideah.util.GHCUtil;
import ideah.util.ProcessLauncher;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.File;
import java.util.StringTokenizer;

public final class HaskellSdkConfigurable implements AdditionalDataConfigurable {

    private static final Logger LOG = Logger.getInstance("ideah.sdk.HaskellSdkConfigurable");

    private final HaskellSdkConfigurableForm myForm;

    private Sdk mySdk;

    public HaskellSdkConfigurable() {
        myForm = new HaskellSdkConfigurableForm();
    }

    @Nullable
    private static String suggestLibPath(@Nullable Sdk sdk) {
        if (sdk == null)
            return null;
        VirtualFile ghcHome = sdk.getHomeDirectory();
        if (ghcHome == null)
            return null;
        String ghcLib = null;
        try {
            String ghcCommandPath = GHCUtil.getGhcCommandPath(ghcHome);
            if (ghcCommandPath == null)
                return null;
            ProcessLauncher getLibdirLauncher = new ProcessLauncher(true, null, ghcCommandPath, "--print-libdir");
            ghcLib = getLibdirLauncher.getStdOut().trim();
        } catch (Exception e) {
            LOG.error(e);
        }
        return ghcLib;
    }

    @Nullable
    private static String getPathFor(String exeName) {
        String path = System.getenv("PATH");
        StringTokenizer stringTokenizer = new StringTokenizer(path, File.pathSeparator);
        while (stringTokenizer.hasMoreTokens()) {
            String dir = stringTokenizer.nextToken();
            File directory = new File(dir);
            if (directory.isDirectory()) {
                File file = new File(directory, exeName);
                if (file.isFile())
                    return file.getAbsolutePath();
            }
        }
        return null;
    }

    @Nullable
    private static String suggestCabalPath(@Nullable String libPath) {
        String cabalExe = GHCUtil.getExeName("cabal");
        if (SystemInfo.isLinux || SystemInfo.isMac) {
            try {
                ProcessLauncher getCabalDir = new ProcessLauncher(true, null, "which", "cabal");
                File cabal = new File(getCabalDir.getStdOut().trim(), cabalExe);
                if (cabal.isFile())
                    return cabal.getPath();
            } catch (Exception e) {
                LOG.error(e.getMessage());
            }
        } else if (SystemInfo.isWindows) {
            if (libPath != null) {
                File cabalDir = new File(libPath + File.separator + "extralibs" + File.separator + "bin");
                File cabal = new File(cabalDir, cabalExe);
                if (cabal.isFile())
                    return cabal.getPath();
            }
            return getPathFor(cabalExe);
        }
        return null;
    }

    public void setSdk(Sdk sdk) {
        mySdk = sdk;
        SdkAdditionalData sdkAdditionalData = sdk.getSdkAdditionalData();
        if (sdkAdditionalData instanceof HaskellSdkAdditionalData) {
            // todo: возможно это будет сохраняться, так что лучше не здесь заполнять, а в форме редактирования?
            HaskellSdkAdditionalData data = (HaskellSdkAdditionalData) sdkAdditionalData;
            String libPath = data.getLibPath();
            if (libPath == null) {
                libPath = suggestLibPath(sdk);
                data.setLibPath(libPath == null ? "" : libPath);
            }
            if (data.getCabalPath() == null) {
                String cabalPath = suggestCabalPath(libPath);
                data.setCabalPath(cabalPath == null ? "" : cabalPath);
            }
            if (data.getGhcOptions() == null) {
                data.setGhcOptions("");
            }
        }
    }

    public JComponent createComponent() {
        return myForm.getContentPanel();
    }

    public boolean isModified() {
        SdkAdditionalData sdkAdditionalData = mySdk.getSdkAdditionalData();
        if (!(sdkAdditionalData instanceof HaskellSdkAdditionalData))
            return true;
        HaskellSdkAdditionalData data = (HaskellSdkAdditionalData) sdkAdditionalData;
        // todo: warning - can be null
        return !(data.getCabalPath().equals(myForm.getCabalPath())
            && data.getGhcOptions().equals(myForm.getGhcOptions())
            && data.getLibPath().equals(myForm.getLibPath()));
    }

    public void apply() {
        String libPath = myForm.getLibPath();
        String cabalPath = myForm.getCabalPath();
        String ghcOptions = myForm.getGhcOptions();
        HaskellSdkAdditionalData newData = new HaskellSdkAdditionalData(libPath, cabalPath, ghcOptions, this);
        final SdkModificator modificator = mySdk.getSdkModificator();
        modificator.setSdkAdditionalData(newData);
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
            public void run() {
                modificator.commitChanges();
            }
        });
    }

    public void reset() {
        SdkAdditionalData data = mySdk.getSdkAdditionalData();
        if (!(data instanceof HaskellSdkAdditionalData))
            return;
        HaskellSdkAdditionalData ghcData = (HaskellSdkAdditionalData) data;
        myForm.init(ghcData.getLibPath(), ghcData.getCabalPath(), ghcData.getGhcOptions());
    }

    public void disposeUIResources() {
    }
}
