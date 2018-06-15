package fi.dy.masa.minihud;

import java.io.File;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.mojang.realmsclient.dto.RealmsServer;
import com.mumfrey.liteloader.Configurable;
import com.mumfrey.liteloader.JoinGameListener;
import com.mumfrey.liteloader.LiteMod;
import com.mumfrey.liteloader.modconfig.ConfigPanel;
import fi.dy.masa.malilib.hotkeys.KeybindEventHandler;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.gui.MiniHudConfigPanel;
import fi.dy.masa.minihud.event.InputEventHandler;
import fi.dy.masa.minihud.event.RenderEventHandler;
import fi.dy.masa.minihud.hotkeys.KeyCallbackToggleHud;
import fi.dy.masa.minihud.util.DataStorage;
import net.minecraft.client.multiplayer.ServerData;
import net.minecraft.network.INetHandler;
import net.minecraft.network.play.server.SPacketJoinGame;

public class LiteModMiniHud implements LiteMod, Configurable, JoinGameListener
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
        Configs.load();
        Configs.Generic.TOGGLE_KEY.getKeybind().setCallback(new KeyCallbackToggleHud());
        KeybindEventHandler.getInstance().registerKeyEventHandler(InputEventHandler.getInstance());
        RenderEventHandler.getInstance().setEnabled(Configs.Generic.ENABLE_BY_DEFAULT.getBooleanValue());
    }

    @Override
    public void upgradeSettings(String version, File configPath, File oldConfigPath)
    {
    }

    @Override
    public void onJoinGame(INetHandler netHandler, SPacketJoinGame joinGamePacket, ServerData serverData, RealmsServer realmsServer)
    {
        DataStorage.getInstance().onWorldLoad();
    }
}
