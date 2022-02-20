package fi.dy.masa.minihud.gui.widget;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.gui.config.ConfigWidgetContext;
import fi.dy.masa.malilib.gui.widget.BaseTextFieldWidget;
import fi.dy.masa.malilib.gui.widget.IntegerTextFieldWidget;
import fi.dy.masa.malilib.gui.widget.KeybindSettingsWidget;
import fi.dy.masa.malilib.gui.widget.button.BooleanConfigButton;
import fi.dy.masa.malilib.gui.widget.button.KeyBindConfigButton;
import fi.dy.masa.malilib.gui.widget.list.entry.config.BaseConfigWidget;
import fi.dy.masa.minihud.config.InfoLine;

public class InfoLineConfigWidget extends BaseConfigWidget<InfoLine>
{
    protected final InfoLine config;
    protected final BaseTextFieldWidget textField;
    protected final BooleanConfigButton booleanButton;
    protected final KeyBindConfigButton hotkeyButton;
    protected final KeybindSettingsWidget settingsWidget;

    protected final ImmutableList<Integer> initialHotkeyValue;
    protected final boolean initialBooleanValue;
    protected final int initialLineOrder;
    protected final String initialLineOrderStringValue;

    public InfoLineConfigWidget(int x, int y, int width, int height, int listIndex, int originalListIndex,
                                InfoLine config, ConfigWidgetContext ctx)
    {
        super(x, y, width, 22, listIndex, originalListIndex, config, ctx);

        this.config = config;
        this.initialBooleanValue = this.config.getBooleanValue();
        this.initialLineOrder = this.config.getLineOrder();
        this.initialLineOrderStringValue = String.valueOf(this.initialLineOrder);
        this.initialHotkeyValue = this.config.getKeyBind().getKeys();

        this.textField = new BaseTextFieldWidget(24, 16);
        this.textField.setTextValidator(new IntegerTextFieldWidget.IntValidator(this.config.getLineOrderConfig().getMinIntegerValue(),
                                                                                this.config.getLineOrderConfig().getMaxIntegerValue()));
        this.textField.setListener((str) -> {
            this.config.getLineOrderConfig().setValueFromString(str);
            this.updateButtonStates();
        });

        this.booleanButton = new BooleanConfigButton(-1, 20, config.getBooleanConfig());
        this.booleanButton.setActionListener(() -> {
            this.config.getBooleanConfig().toggleBooleanValue();
            this.updateButtonStates();
        });

        this.hotkeyButton = new KeyBindConfigButton(120, 20, config.getKeyBind(), ctx.getKeybindEditingScreen());
        this.hotkeyButton.setValueChangeListener(this::updateButtonStates);

        this.settingsWidget = new KeybindSettingsWidget(config.getKeyBind(), config.getDisplayName());

        this.resetButton.setActionListener(() -> {
            this.config.resetToDefault();
            this.textField.setText(String.valueOf(this.config.getLineOrderConfig().getIntegerValue()));
            this.updateButtonStates();
        });
    }

    @Override
    public void reAddSubWidgets()
    {
        super.reAddSubWidgets();

        this.addWidget(this.textField);
        this.addWidget(this.booleanButton);
        this.addWidget(this.hotkeyButton);
        this.addWidget(this.settingsWidget);
        this.addWidget(this.resetButton);
    }

    @Override
    public void updateSubWidgetsToGeometryChanges()
    {
        super.updateSubWidgetsToGeometryChanges();

        int y = this.getY() + 1;
        int elementWidth = this.getElementWidth();

        this.textField.setPosition(this.getElementsStartPosition(), y + 2);
        this.textField.setText(String.valueOf(this.config.getLineOrderConfig().getIntegerValue()));

        this.booleanButton.setPosition(this.textField.getRight() + 2, y);

        int w = elementWidth - this.booleanButton.getWidth() - 24 - 20 - 6;
        this.hotkeyButton.setWidth(w);
        this.hotkeyButton.setPosition(this.booleanButton.getRight() + 2, y);

        this.settingsWidget.setPosition(this.hotkeyButton.getRight() + 2, y);

        this.updateResetButton(this.settingsWidget.getRight() + 4, y);
        this.updateButtonStates();
    }

    @Override
    public void onAboutToDestroy()
    {
        String text = this.textField.getText();

        if (text.equals(this.initialLineOrderStringValue) == false)
        {
            this.config.getLineOrderConfig().setValueFromString(text);
        }
    }

    @Override
    public boolean wasModified()
    {
        return this.config.getBooleanValue() != this.initialBooleanValue ||
               this.config.getLineOrder() != this.initialLineOrder ||
               this.config.getKeyBind().getKeys().equals(this.initialHotkeyValue) == false;
    }

    protected void updateButtonStates()
    {
        this.booleanButton.setEnabled(this.config.getBooleanConfig().isLocked() == false);
        this.booleanButton.updateButtonState();
        this.booleanButton.updateHoverStrings();

        this.hotkeyButton.updateButtonState();
        this.hotkeyButton.updateHoverStrings();

        this.resetButton.setEnabled(this.config.isModified() && this.config.getBooleanConfig().isLocked() == false);
    }
}
