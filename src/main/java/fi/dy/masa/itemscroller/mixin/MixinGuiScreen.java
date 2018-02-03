package fi.dy.masa.itemscroller.mixin;

import org.lwjgl.input.Keyboard;
import org.lwjgl.input.Mouse;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.itemscroller.event.InputEventHandler;
import fi.dy.masa.itemscroller.event.RenderEventHandler;
import net.minecraft.client.gui.Gui;
import net.minecraft.client.gui.GuiScreen;

@Mixin(GuiScreen.class)
public abstract class MixinGuiScreen extends Gui
{
    @Shadow public abstract void handleMouseInput();
    @Shadow public abstract void handleKeyboardInput();

    @Inject(method = "drawDefaultBackground()V", at = @At("RETURN"))
    protected void onDrawDefaultBackgroundPost(CallbackInfo ci)
    {
        RenderEventHandler.instance().onDrawBackgroundPost();
    }

    @Inject(method = "handleInput()V", at = @At("HEAD"), cancellable = true)
    protected void onHandleInput(CallbackInfo ci)
    {
        if (Mouse.isCreated())
        {
            while (Mouse.next())
            {
                if (InputEventHandler.instance().onMouseInput() == false)
                    this.handleMouseInput();
            }
        }

        if (Keyboard.isCreated())
        {
            while (Keyboard.next())
            {
                if (InputEventHandler.instance().onKeyInput() == false)
                    this.handleKeyboardInput();
            }
        }
    }
}
