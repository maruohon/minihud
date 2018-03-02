package fi.dy.masa.minihud;

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
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.ConfigsGeneric;
import fi.dy.masa.minihud.config.gui.MiniHudConfigPanel;
import fi.dy.masa.minihud.event.RenderEventHandler;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.ServerData;
import net.minecraft.client.settings.KeyBinding;
import net.minecraft.network.INetHandler;
import net.minecraft.network.play.server.SPacketJoinGame;

public class LiteModMiniHud implements LiteMod, Configurable, InitCompleteListener, JoinGameListener
{
    public static final KeyBinding KEY_TOGGLE_MODE = new KeyBinding("minihud.key.togglemode", Keyboard.KEY_H, "category.minihud");
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
        Configs.load();
        RenderEventHandler.getInstance().setEnabled(ConfigsGeneric.ENABLE_BY_DEFAULT.getBooleanValue());
    }

    @Override
    public void upgradeSettings(String version, File configPath, File oldConfigPath)
    {
    }

    @Override
    public void onInitCompleted(Minecraft minecraft, LiteLoader loader)
    {
        LiteLoader.getInput().registerKeyBinding(KEY_TOGGLE_MODE);
    }

    @Override
    public void onJoinGame(INetHandler netHandler, SPacketJoinGame joinGamePacket, ServerData serverData, RealmsServer realmsServer)
    {
        RenderEventHandler.getInstance().onWorldLoad();
    }
}
