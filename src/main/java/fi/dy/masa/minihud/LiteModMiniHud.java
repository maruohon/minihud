package fi.dy.masa.minihud;

import java.io.File;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.google.common.collect.ImmutableList;
import com.mojang.realmsclient.dto.RealmsServer;
import com.mumfrey.liteloader.Configurable;
import com.mumfrey.liteloader.JoinGameListener;
import com.mumfrey.liteloader.LiteMod;
import com.mumfrey.liteloader.PluginChannelListener;
import com.mumfrey.liteloader.Tickable;
import com.mumfrey.liteloader.modconfig.ConfigPanel;
import fi.dy.masa.malilib.config.ConfigManager;
import fi.dy.masa.malilib.event.InputEventHandler;
import fi.dy.masa.malilib.event.RenderEventHandler;
import fi.dy.masa.malilib.hotkeys.IHotkeyCallback;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeyAction;
import fi.dy.masa.malilib.hotkeys.KeyCallbackToggleBoolean;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.gui.MiniHudConfigPanel;
import fi.dy.masa.minihud.event.InputHandler;
import fi.dy.masa.minihud.event.RenderHandler;
import fi.dy.masa.minihud.gui.GuiConfigs;
import fi.dy.masa.minihud.util.DataStorage;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.ServerData;
import net.minecraft.network.INetHandler;
import net.minecraft.network.PacketBuffer;
import net.minecraft.network.play.server.SPacketJoinGame;

public class LiteModMiniHud implements LiteMod, Configurable, JoinGameListener, PluginChannelListener, Tickable
{
    public static final Logger logger = LogManager.getLogger(Reference.MOD_ID);

    public static final String CHANNEL_CARPET_CLIENT = "CarpetClient";

    private final ImmutableList<String> pluginChannels = ImmutableList.of(CHANNEL_CARPET_CLIENT);

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
        Configs.loadFromFile();

        ConfigManager.getInstance().registerConfigHandler(Reference.MOD_ID, new Configs());
        InputEventHandler.getInstance().registerKeybindProvider(InputHandler.getInstance());

        RenderHandler renderer = RenderHandler.getInstance();
        RenderEventHandler.getInstance().registerGameOverlayRenderer(renderer);
        RenderEventHandler.getInstance().registerTooltipLastRenderer(renderer);
        RenderEventHandler.getInstance().registerWorldLastRenderer(renderer);

        Configs.Generic.OPEN_CONFIG_GUI.getKeybind().setCallback(new CallbackOpenConfigGui());
        Configs.Generic.TOGGLE_KEY.getKeybind().setCallback(new KeyCallbackToggleBoolean(Configs.Generic.ENABLED));
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

    @Override
    public void onTick(Minecraft mc, float partialTicks, boolean inGame, boolean clock)
    {
        RenderHandler.getInstance().updateData(mc);
    }

    @Override
    public void onCustomPayload(String channel, PacketBuffer data)
    {
        if (CHANNEL_CARPET_CLIENT.equals(channel))
        {
            DataStorage.getInstance().updateStructureDataFromServer(data);
        }
    }

    @Override
    public List<String> getChannels()
    {
        return this.pluginChannels;
    }

    public static class CallbackOpenConfigGui implements IHotkeyCallback
    {
        @Override
        public boolean onKeyAction(KeyAction action, IKeybind key)
        {
            Minecraft.getMinecraft().displayGuiScreen(new GuiConfigs());
            return true;
        }
    }
}
