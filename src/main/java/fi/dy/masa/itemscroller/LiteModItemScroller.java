package fi.dy.masa.itemscroller;

import java.io.File;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.mumfrey.liteloader.Configurable;
import com.mumfrey.liteloader.InitCompleteListener;
import com.mumfrey.liteloader.LiteMod;
import com.mumfrey.liteloader.Tickable;
import com.mumfrey.liteloader.core.LiteLoader;
import com.mumfrey.liteloader.modconfig.ConfigPanel;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.config.ItemScrollerConfigPanel;
import fi.dy.masa.itemscroller.event.InputHandler;
import fi.dy.masa.itemscroller.event.KeybindCallbacks;
import fi.dy.masa.itemscroller.event.WorldLoadListener;
import fi.dy.masa.malilib.config.ConfigManager;
import fi.dy.masa.malilib.event.InputEventHandler;
import fi.dy.masa.malilib.event.WorldLoadHandler;
import net.minecraft.client.Minecraft;

public class LiteModItemScroller implements LiteMod, Configurable, InitCompleteListener, Tickable
{
    public static final Logger logger = LogManager.getLogger(Reference.MOD_ID);

    public LiteModItemScroller()
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
        return ItemScrollerConfigPanel.class;
    }

    @Override
    public void init(File configPath)
    {
        Configs.loadFromFile();
        ConfigManager.getInstance().registerConfigHandler(Reference.MOD_ID, new Configs());

        InputHandler handler = new InputHandler();
        InputEventHandler.getKeybindManager().registerKeybindProvider(handler);
        InputEventHandler.getInputManager().registerKeyboardInputHandler(handler);
        InputEventHandler.getInputManager().registerMouseInputHandler(handler);

        WorldLoadListener listener = new WorldLoadListener();
        WorldLoadHandler.getInstance().registerWorldLoadPreHandler(listener);
        WorldLoadHandler.getInstance().registerWorldLoadPostHandler(listener);
    }

    @Override
    public void upgradeSettings(String version, File configPath, File oldConfigPath)
    {
    }

    @Override
    public void onInitCompleted(Minecraft mc, LiteLoader loader)
    {
        KeybindCallbacks.getInstance().setCallbacks();
    }

    @Override
    public void onTick(Minecraft mc, float partialTicks, boolean inGame, boolean clock)
    {
        KeybindCallbacks.getInstance().onTick(mc);
    }
}
