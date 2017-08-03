package fi.dy.masa.minihud;

import org.lwjgl.input.Keyboard;

public class Reference
{
    public static final String MOD_ID = "minihud";
    public static final String MOD_NAME = "Mini HUD";
    public static final String MOD_VERSION = "@MOD_VERSION@";
    public static final String FINGERPRINT = "2b03e1423915a189b8094816baa18f239d576dff";

    public static final String KEYBIND_CATEGORY_MINIHUD = "category." + Reference.MOD_ID;
    public static final String KEYBIND_NAME_TOGGLE_MODE = Reference.MOD_ID + ".key.togglemode";
    public static final int DEFAULT_KEYBIND_TOGGLE_MODE = Keyboard.KEY_H;
}
