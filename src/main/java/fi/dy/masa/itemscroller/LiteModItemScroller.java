package fi.dy.masa.itemscroller;

import java.io.File;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.lwjgl.input.Keyboard;
import com.mojang.realmsclient.dto.RealmsServer;
import com.mumfrey.liteloader.Configurable;
import com.mumfrey.liteloader.InitCompleteListener;
import com.mumfrey.liteloader.JoinGameListener;
import com.mumfrey.liteloader.LiteMod;
import com.mumfrey.liteloader.core.LiteLoader;
import com.mumfrey.liteloader.modconfig.ConfigPanel;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.config.ItemScrollerConfigPanel;
import fi.dy.masa.itemscroller.event.InputEventHandler;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.ServerData;
import net.minecraft.client.settings.KeyBinding;
import net.minecraft.network.INetHandler;
import net.minecraft.network.play.server.SPacketJoinGame;

public class LiteModItemScroller implements LiteMod, Configurable, InitCompleteListener, JoinGameListener
{
    public static final KeyBinding KEY_DISABLE = new KeyBinding("itemscroller.desc.toggledisable", Keyboard.KEY_N, "itemscroller.category");
    public static final KeyBinding KEY_RECIPE = new KeyBinding("itemscroller.desc.recipe", Keyboard.KEY_S, "itemscroller.category");

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
        Configs.load();
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
    public void onJoinGame(INetHandler netHandler, SPacketJoinGame joinGamePacket, ServerData serverData, RealmsServer realmsServer)
    {
        InputEventHandler.getInstance().onWorldChanged();
    }
}
