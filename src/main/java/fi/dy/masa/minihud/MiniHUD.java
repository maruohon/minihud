package fi.dy.masa.minihud;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.dimdev.rift.listener.client.ClientTickable;
import org.dimdev.rift.listener.client.OverlayRenderer;
import org.dimdev.riftloader.listener.InitializationListener;
import org.spongepowered.asm.launch.MixinBootstrap;
import org.spongepowered.asm.mixin.Mixins;
import fi.dy.masa.malilib.config.ConfigManager;
import fi.dy.masa.malilib.hotkeys.IHotkeyCallback;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeyAction;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.event.RenderHandler;
import fi.dy.masa.minihud.gui.GuiConfigs;
import net.minecraft.client.Minecraft;

public class MiniHUD implements ClientTickable, InitializationListener, OverlayRenderer
{
    public static final Logger logger = LogManager.getLogger(Reference.MOD_ID);

    public MiniHUD()
    {
    }

    @Override
    public void onInitialization()
    {
        MixinBootstrap.init();
        Mixins.addConfiguration("mixins.minihud.json");

        ConfigManager.getInstance().registerConfigHandler(Reference.MOD_ID, new Configs());
        //InputEventHandler.getInstance().registerKeybindProvider(InputHandler.getInstance());

        //RenderEventHandler.getInstance().registerGameOverlayRenderer(RenderHandler.getInstance());
        //RenderEventHandler.getInstance().registerWorldLastRenderer(RenderHandler.getInstance());

        //Configs.Generic.OPEN_CONFIG_GUI.getKeybind().setCallback(new CallbackOpenConfigGui());
        //Configs.Generic.TOGGLE_KEY.getKeybind().setCallback(new KeyCallbackToggleBoolean(Configs.Generic.ENABLED));
    }

    @Override
    public void renderOverlay()
    {
        float partialTicks = Minecraft.getInstance().getRenderPartialTicks();
        RenderHandler.getInstance().onRenderGameOverlayPost(partialTicks);
    }

    /*
    @Override
    public void init(File configPath)
    {
        Configs.loadFromFile();

        ConfigManager.getInstance().registerConfigHandler(Reference.MOD_ID, new Configs());
        InputEventHandler.getInstance().registerKeybindProvider(InputHandler.getInstance());

        RenderEventHandler.getInstance().registerGameOverlayRenderer(RenderHandler.getInstance());
        RenderEventHandler.getInstance().registerWorldLastRenderer(RenderHandler.getInstance());

        Configs.Generic.OPEN_CONFIG_GUI.getKeybind().setCallback(new CallbackOpenConfigGui());
        Configs.Generic.TOGGLE_KEY.getKeybind().setCallback(new KeyCallbackToggleBoolean(Configs.Generic.ENABLED));
    }
    */

    // TODO
    /*
    @Override
    public void onJoinGame(INetHandler netHandler, SPacketJoinGame joinGamePacket, ServerData serverData, RealmsServer realmsServer)
    {
        DataStorage.getInstance().onWorldLoad();
    }
    */

    @Override
    public void clientTick(Minecraft mc)
    {
        RenderHandler.getInstance().updateData(mc);
    }

    public static class CallbackOpenConfigGui implements IHotkeyCallback
    {
        @Override
        public boolean onKeyAction(KeyAction action, IKeybind key)
        {
            Minecraft.getInstance().displayGuiScreen(new GuiConfigs());
            return true;
        }
    }
}
