package minihud;

import malilib.config.JsonModConfig;
import malilib.config.JsonModConfig.ConfigDataUpdater;
import malilib.config.util.ConfigUpdateUtils.KeyBindSettingsResetter;
import malilib.event.InitializationHandler;
import malilib.gui.config.ConfigSearchInfo;
import malilib.registry.Registry;
import minihud.config.ConfigCallbacks;
import minihud.config.Configs;
import minihud.config.InfoLineToggle;
import minihud.config.RendererToggle;
import minihud.config.StructureToggle;
import minihud.event.ClientTickHandler;
import minihud.event.ClientWorldChangeHandler;
import minihud.event.RenderHandler;
import minihud.gui.ConfigScreen;
import minihud.gui.widget.InfoLineConfigWidget;
import minihud.gui.widget.RendererToggleConfigWidget;
import minihud.gui.widget.StructureToggleConfigWidget;
import minihud.gui.widget.info.RendererToggleConfigStatusWidget;
import minihud.gui.widget.info.StructureRendererConfigStatusWidget;
import minihud.input.MiniHudHotkeyProvider;
import minihud.network.servux.ServuxInfoSubDataPacketHandler;
import minihud.network.servux.ServuxInfoSubRegistrationPacketHandler;

public class InitHandler implements InitializationHandler
{
    @Override
    public void registerModHandlers()
    {
        // Reset all KeyBindSettings when updating to the first post-malilib-refactor version
        ConfigDataUpdater updater = new KeyBindSettingsResetter(MiniHudHotkeyProvider.INSTANCE::getAllHotkeys, 0);
        Registry.CONFIG_MANAGER.registerConfigHandler(JsonModConfig.createJsonModConfig(Reference.MOD_INFO, Configs.CURRENT_VERSION, Configs.CATEGORIES, updater));

        Registry.CONFIG_SCREEN.registerConfigScreenFactory(Reference.MOD_INFO, ConfigScreen::create);
        Registry.CONFIG_TAB.registerConfigTabProvider(Reference.MOD_INFO, ConfigScreen::getConfigTabs);

        Registry.CONFIG_WIDGET.registerConfigWidgetFactory(InfoLineToggle.class, InfoLineConfigWidget::new);
        Registry.CONFIG_WIDGET.registerConfigWidgetFactory(RendererToggle.class, RendererToggleConfigWidget::new);
        Registry.CONFIG_WIDGET.registerConfigWidgetFactory(StructureToggle.class, StructureToggleConfigWidget::new);

        Registry.CONFIG_WIDGET.registerConfigSearchInfo(InfoLineToggle.class, new ConfigSearchInfo<InfoLineToggle>(true, true).setBooleanStorageGetter(InfoLineToggle::getBooleanConfig).setKeyBindGetter(InfoLineToggle::getKeyBind));
        Registry.CONFIG_WIDGET.registerConfigSearchInfo(RendererToggle.class, new ConfigSearchInfo<RendererToggle>(true, true).setBooleanStorageGetter(RendererToggle::getBooleanConfig).setKeyBindGetter(RendererToggle::getKeyBind));
        Registry.CONFIG_WIDGET.registerConfigSearchInfo(StructureToggle.class, new ConfigSearchInfo<StructureToggle>(true, true).setBooleanStorageGetter(StructureToggle::getBooleanConfig).setKeyBindGetter(StructureToggle::getKeyBind));

        Registry.CONFIG_STATUS_WIDGET.registerConfigStatusWidgetFactory(RendererToggle.class, RendererToggleConfigStatusWidget::new, "minihud:csi_value_renderer_toggle");
        Registry.CONFIG_STATUS_WIDGET.registerConfigStatusWidgetFactory(StructureToggle.class, StructureRendererConfigStatusWidget::new, "minihud:csi_value_structure_toggle");

        Registry.HOTKEY_MANAGER.registerHotkeyProvider(MiniHudHotkeyProvider.INSTANCE);

        RenderHandler renderer = RenderHandler.INSTANCE;
        Registry.RENDER_EVENT_DISPATCHER.registerGameOverlayRenderer(renderer);
        Registry.RENDER_EVENT_DISPATCHER.registerTooltipPostRenderer(renderer);
        Registry.RENDER_EVENT_DISPATCHER.registerWorldPostRenderer(renderer);

        Registry.CLIENT_WORLD_CHANGE_EVENT_DISPATCHER.registerClientWorldChangeHandler(new ClientWorldChangeHandler());

        Registry.TICK_EVENT_DISPATCHER.registerClientTickHandler(new ClientTickHandler());

        Registry.CLIENT_PACKET_CHANNEL_HANDLER.registerClientChannelHandler(ServuxInfoSubRegistrationPacketHandler.INSTANCE);
        Registry.CLIENT_PACKET_CHANNEL_HANDLER.registerClientChannelHandler(ServuxInfoSubDataPacketHandler.INSTANCE);

        MiniHUDActions.init();
        ConfigCallbacks.init();
    }
}
