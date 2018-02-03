package fi.dy.masa.itemscroller;

import java.io.File;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.lwjgl.input.Keyboard;
import com.mumfrey.liteloader.InitCompleteListener;
import com.mumfrey.liteloader.LiteMod;
import com.mumfrey.liteloader.RenderListener;
import com.mumfrey.liteloader.api.WorldObserver;
import com.mumfrey.liteloader.core.LiteLoader;
import com.mumfrey.liteloader.modconfig.ConfigStrategy;
import com.mumfrey.liteloader.modconfig.ExposableOptions;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.event.InputEventHandler;
import fi.dy.masa.itemscroller.event.RenderEventHandler;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.settings.KeyBinding;
import net.minecraft.world.World;

@ExposableOptions(strategy = ConfigStrategy.Versioned, filename="itemscroller.json")
public class LiteModItemScroller implements LiteMod, InitCompleteListener, RenderListener, WorldObserver
{
    public static final KeyBinding KEY_DISABLE = new KeyBinding("itemscroller.desc.toggledisable", Keyboard.KEY_N, "itemscroller.category");
    public static final KeyBinding KEY_RECIPE = new KeyBinding("itemscroller.desc.recipe", Keyboard.KEY_S, "itemscroller.category");

    public static final Logger logger = LogManager.getLogger(Reference.MOD_ID);
    public static File configDir;

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
    public void init(File configPath)
    {
        //System.out.printf("************** configPath: %s *******************\n", configPath.getPath());
        configDir = configPath;
        Configs.loadConfigsFromFile(new File(configPath, Reference.MOD_ID + ".cfg"));
    }

    @Override
    public void upgradeSettings(String version, File configPath, File oldConfigPath)
    {
    }

    @Override
    public void onInitCompleted(Minecraft minecraft, LiteLoader loader)
    {
        LiteLoader.getInput().registerKeyBinding(KEY_DISABLE);
        LiteLoader.getInput().registerKeyBinding(KEY_RECIPE);
    }

    @Override
    public void onRender()
    {
    }

    @Override
    public void onRenderGui(GuiScreen currentScreen)
    {
        RenderEventHandler.instance().onDrawScreen();
    }

    @Override
    public void onSetupCameraTransform()
    {
    }

    @Override
    public void onWorldChanged(World world)
    {
        InputEventHandler.instance().onWorldChanged();
    }
}
