package fi.dy.masa.minihud.gui.widget;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.gui.widget.button.BooleanConfigButton;
import fi.dy.masa.malilib.gui.widget.button.KeyBindConfigButton;
import fi.dy.masa.malilib.gui.config.ConfigWidgetContext;
import fi.dy.masa.malilib.gui.widget.KeybindSettingsWidget;
import fi.dy.masa.malilib.gui.widget.list.entry.config.BaseConfigOptionWidget;
import fi.dy.masa.minihud.config.RendererToggle;

public class RendererToggleConfigWidget extends BaseConfigOptionWidget<RendererToggle>
{
    protected final RendererToggle config;
    protected final ImmutableList<Integer> initialHotkeyValue;
    protected final BooleanConfigButton booleanButton;
    protected final KeyBindConfigButton hotkeyButton;
    protected final KeybindSettingsWidget settingsWidget;
    protected final boolean initialBooleanValue;

    public RendererToggleConfigWidget(int x, int y, int width, int height, int listIndex, int originalListIndex,
                                       RendererToggle config, ConfigWidgetContext ctx)
    {
        super(x, y, width, 22, listIndex, originalListIndex, config, ctx);

        this.config = config;
        this.initialBooleanValue = config.isRendererEnabled();
        this.initialHotkeyValue = config.getKeyBind().getKeys();

        this.booleanButton = new BooleanConfigButton(x, y + 1, 40, 20, config.getBooleanConfig());
        this.booleanButton.setActionListener((btn, mbtn) -> this.resetButton.setEnabled(this.config.isModified()));

        this.hotkeyButton = new KeyBindConfigButton(x, y + 1, 120, 20, config.getKeyBind(), ctx.gui);
        this.hotkeyButton.setValueChangeListener(() -> this.resetButton.setEnabled(this.config.isModified()));

        this.settingsWidget = new KeybindSettingsWidget(x, y, 20, 20, config.getKeyBind(),
                                                        config.getDisplayName(), ctx.gui.getDialogHandler());

        this.resetButton.setActionListener((btn, mbtn) -> {
            this.config.resetToDefault();
            this.resetButton.setEnabled(this.config.isModified());
            this.booleanButton.updateDisplayString();
            this.hotkeyButton.updateDisplayString();
        });
    }

    @Override
    public void reAddSubWidgets()
    {
        super.reAddSubWidgets();

        int x = this.getX() + this.getMaxLabelWidth() + 10;
        int y = this.getY() + 1;
        int elementWidth = this.getElementWidth();

        this.booleanButton.setPosition(x, y);

        int w = this.booleanButton.getWidth();
        x += w + 2;
        this.hotkeyButton.setPosition(x, y);

        w = elementWidth - w - 20 - 4;
        this.hotkeyButton.setWidth(w);

        x += w + 2;
        this.settingsWidget.setPosition(x, y);

        x += this.settingsWidget.getWidth() + 4;
        this.updateResetButton(x, y, this.config);

        this.addWidget(this.booleanButton);
        this.addWidget(this.hotkeyButton);
        this.addWidget(this.settingsWidget);
        this.addWidget(this.resetButton);
    }

    @Override
    public boolean wasModified()
    {
        return this.config.isRendererEnabled() != this.initialBooleanValue ||
               this.config.getKeyBind().getKeys().equals(this.initialHotkeyValue) == false;
    }
}
