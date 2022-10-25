package minihud;

import java.io.File;
import com.mumfrey.liteloader.Configurable;
import com.mumfrey.liteloader.LiteMod;
import com.mumfrey.liteloader.modconfig.ConfigPanel;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import malilib.registry.Registry;
import minihud.gui.MiniHudConfigPanel;

public class LiteModMiniHud implements LiteMod, Configurable
{
    public static final Logger logger = LogManager.getLogger(Reference.MOD_ID);

    public LiteModMiniHud()
    {
    }

    @Override
    public String getName()
    {
        return Reference.MOD_NAME;
    }

    @Override
    public String getVersion()
    {
        return Reference.MOD_VERSION;
    }

    @Override
    public Class<? extends ConfigPanel> getConfigPanelClass()
    {
        return MiniHudConfigPanel.class;
    }

    @Override
    public void init(File configPath)
    {
        Registry.INITIALIZATION_DISPATCHER.registerInitializationHandler(new InitHandler());
    }

    @Override
    public void upgradeSettings(String version, File configPath, File oldConfigPath)
    {
    }
}
