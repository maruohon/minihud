package fi.dy.masa.minihud.config;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.IConfigBoolean;
import fi.dy.masa.malilib.config.options.ConfigBoolean;
import fi.dy.masa.malilib.config.options.ConfigColor;
import fi.dy.masa.malilib.config.options.ConfigHotkey;
import fi.dy.masa.malilib.hotkeys.IHotkey;
import fi.dy.masa.minihud.util.DataStorage;

public enum StructureToggle
{
    OVERLAY_STRUCTURE_BASTION_REMNANT   ("Bastion Remnant", "", "#302171F5", "#302171F5", "Toggle Bastion Remnant structure bounding boxes rendering", "Bastion Remnant"),
    OVERLAY_STRUCTURE_BURIED_TREASURE   ("Buried Treasure", "", "#302298E6", "#302298E6", "Toggle Buried Treasure structure bounding boxes rendering", "Buried Treasure"),
    OVERLAY_STRUCTURE_DESERT_PYRAMID    ("Desert Pyramid",  "", "#30FFFF00", "#30FFFF00", "Toggle Desert Pyramid structure bounding boxes rendering", "Desert Pyramid"),
    OVERLAY_STRUCTURE_END_CITY          ("End City",        "", "#30EB07EB", "#30EB07EB", "Toggle End City structure bounding boxes rendering", "End City"),
    OVERLAY_STRUCTURE_IGLOO             ("Igloo",           "", "#300FAFE4", "#300FAFE4", "Toggle Igloo structure bounding boxes rendering", "Igloo structures"),
    OVERLAY_STRUCTURE_JUNGLE_TEMPLE     ("Jungle Temple",   "", "#3099FF00", "#3099FF00", "Toggle Jungle Temple structure bounding boxes rendering", "Jungle Temple"),
    OVERLAY_STRUCTURE_MANSION           ("Mansion",         "", "#30FF6500", "#30FF6500", "Toggle Mansion structure bounding boxes rendering", "Mansion structures"),
    OVERLAY_STRUCTURE_MINESHAFT         ("Mineshaft",       "", "#30F8D650", "#30F8D650", "Toggle Mineshaft structure bounding boxes rendering", "Mineshaft structures"),
    OVERLAY_STRUCTURE_NETHER_FORTRESS   ("Nether Fortress", "", "#30FC381D", "#30FC381D", "Toggle Nether Fortress structure bounding boxes rendering", "Nether Fortress"),
    OVERLAY_STRUCTURE_NETHER_FOSSIL     ("Nether Fossil",   "", "#30868E99", "#30868E99", "Toggle Nether Fossil structure bounding boxes rendering", "Nether Fossil"),
    OVERLAY_STRUCTURE_OCEAN_MONUMENT    ("Ocean Monument",  "", "#3029E6EF", "#3029E6EF", "Toggle Ocean Monument structure bounding boxes rendering", "Ocean Monument"),
    OVERLAY_STRUCTURE_OCEAN_RUIN        ("Ocean Ruin",      "", "#300FAD83", "#300FAD83", "Toggle Ocean Ruin structure bounding boxes rendering", "Ocean Ruin"),
    OVERLAY_STRUCTURE_PILLAGER_OUTPOST  ("Pillager Outpost","", "#300FAD83", "#300FAD83", "Toggle Pillager Outpost structure bounding boxes rendering", "Pillager Outpost"),
    OVERLAY_STRUCTURE_RUINED_PORTAL     ("Ruined Portal",   "", "#309F03D3", "#309F03D3", "Toggle Ruined Portal structure bounding boxes rendering", "Ruined Portal"),
    OVERLAY_STRUCTURE_SHIPWRECK         ("Shipwreck",       "", "#30EB1995", "#30EB1995", "Toggle Shipwreck structure bounding boxes rendering", "Shipwreck"),
    OVERLAY_STRUCTURE_STRONGHOLD        ("Stronghold",      "", "#30009999", "#30009999", "Toggle Stronghold structure bounding boxes rendering", "Stronghold"),
    OVERLAY_STRUCTURE_VILLAGE           ("Village",         "", "#3054CB4E", "#3054CB4E", "Toggle Village structure bounding boxes rendering\nNOTE: This is the Village world gen structures!\nThis is not the Village data you use for iron farms etc.!", "Village"),
    OVERLAY_STRUCTURE_WITCH_HUT         ("Witch Hut",       "", "#30BE1DFC", "#300099FF", "Toggle Witch Hut structure bounding boxes rendering", "Witch Hut"),

    OVERLAY_STRUCTURE_UNKNOWN           ("Unknown",         "", "#50FFFFFF", "#50FFFFFF", "Toggle Unknown structure bounding boxes rendering", "Unknown Structure");

    public static final ImmutableList<StructureToggle> VALUES = ImmutableList.copyOf(values());
    public static final ImmutableList<IConfigBoolean> TOGGLE_CONFIGS = ImmutableList.copyOf(VALUES.stream().map(StructureToggle::getToggleOption).toList());
    public static final ImmutableList<IHotkey> HOTKEY_CONFIGS = ImmutableList.copyOf(VALUES.stream().map(StructureToggle::getHotkey).toList());
    public static final ImmutableList<ConfigColor> COLOR_CONFIGS = getColorConfigs();

    private final ConfigBoolean toggleOption;
    private final ConfigColor colorMain;
    private final ConfigColor colorComponents;
    private final IHotkey hotkey;

    StructureToggle(String name, String defaultHotkey, String colorMain, String colorComponents, String comment, String prettyName)
    {
        this.toggleOption    = new ConfigBoolean(name, false, comment, prettyName);
        this.colorMain       = new ConfigColor(name +  " Main", colorMain, prettyName + " full box");
        this.colorComponents = new ConfigColor(name + " Components", colorComponents, prettyName + " components");
        this.hotkey          = new ConfigHotkey("Toggle " + name, defaultHotkey, comment);

        this.hotkey.getKeybind().setCallback((action, key) -> { this.toggleOption.toggleBooleanValue(); return true; });
        this.toggleOption.setValueChangeCallback((config) -> DataStorage.getInstance().setStructuresNeedUpdating());
    }

    public IConfigBoolean getToggleOption()
    {
        return this.toggleOption;
    }

    public ConfigColor getColorMain()
    {
        return this.colorMain;
    }

    public ConfigColor getColorComponents()
    {
        return this.colorComponents;
    }

    public IHotkey getHotkey()
    {
        return this.hotkey;
    }

    private static ImmutableList<ConfigColor> getColorConfigs()
    {
        ImmutableList.Builder<ConfigColor> builder = ImmutableList.builder();

        for (StructureToggle toggle : VALUES)
        {
            builder.add(toggle.getColorMain());
            builder.add(toggle.getColorComponents());
        }

        return builder.build();
    }
}
