package fi.dy.masa.itemscroller;

import java.util.List;

import org.lwjgl.input.Mouse;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.Slot;
import net.minecraft.inventory.SlotCrafting;
import net.minecraft.item.ItemStack;

import net.minecraftforge.client.event.GuiScreenEvent.MouseInputEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class InputEventHandler
{
    private ItemStack[] originalStacks = new ItemStack[0];

    @SubscribeEvent
    public void onMouseInputEvent(MouseInputEvent.Pre event)
    {
        int dWheel = Mouse.getEventDWheel();

        if (event.gui instanceof GuiContainer && dWheel != 0)
        {
            this.tryMoveItems((GuiContainer)event.gui, dWheel < 0);
        }
    }

    private void tryMoveItems(GuiContainer gui, boolean reverse)
    {
        Slot slot = gui.getSlotUnderMouse();
        if (slot == null || slot.getHasStack() == false || gui.mc.thePlayer.inventory.getItemStack() != null)
        {
            return;
        }

        Container container = gui.inventorySlots;
        ItemStack stackOrig = slot.getStack();

        if (reverse == false)
        {
            if (slot.canTakeStack(gui.mc.thePlayer) == false)
            {
                return;
            }

            this.originalStacks = this.getOriginalStacks(container);
            ItemStack stack = stackOrig.copy();
            stack.stackSize = 1;

            // Try to move the temporary single-item stack via the shift-click handler method
            slot.putStack(stack);
            container.transferStackInSlot(gui.mc.thePlayer, slot.slotNumber);

            // Successfully moved the item somewhere, now we want to check where it went
            if (slot.getHasStack() == false)
            {
                int targetSlot = this.getTargetSlot(container);

                // Found where the item went
                if (targetSlot >= 0)
                {
                    // Remove the dummy item from the target slot (on the client side)
                    container.inventorySlots.get(targetSlot).decrStackSize(1);

                    // Restore the original stack to the slot under the cursor (on the client side)
                    slot.putStack(stackOrig);

                    // Do the slot clicks to actually move the items (on the server side)
                    this.clickSlots(container, gui.mc, slot.slotNumber, targetSlot);
                    return;
                }
            }

            // Restore the original stack to the slot under the cursor (on the client side)
            slot.putStack(stackOrig);
        }
        else if (reverse == true && stackOrig.stackSize < slot.getSlotStackLimit())
        {
            for (Slot slotTmp : container.inventorySlots)
            {
                if (slotTmp.isHere(slot.inventory, slotTmp.getSlotIndex()) == false && (slotTmp instanceof SlotCrafting) == false &&
                    slotTmp.getHasStack() == true && slotTmp.canTakeStack(gui.mc.thePlayer) == true)
                {
                    ItemStack stackTmp = slotTmp.getStack();
                    if (ItemStack.areItemsEqual(stackOrig, stackTmp) == true && ItemStack.areItemStackTagsEqual(stackOrig, stackTmp) == true)
                    {
                        this.clickSlots(container, gui.mc, slotTmp.slotNumber, slot.slotNumber);
                        break;
                    }
                }
            }
        }
    }

    private ItemStack[] getOriginalStacks(Container container)
    {
        ItemStack[] originalStacks = new ItemStack[container.inventorySlots.size()];

        for (int i = 0; i < originalStacks.length; i++)
        {
            originalStacks[i] = ItemStack.copyItemStack(container.inventorySlots.get(i).getStack());
        }

        return originalStacks;
    }

    private int getTargetSlot(Container container)
    {
        List<Slot> slots = container.inventorySlots;

        for (int i = 0; i < this.originalStacks.length; i++)
        {
            ItemStack stackOrig = this.originalStacks[i];
            ItemStack stackNew = slots.get(i).getStack();

            if ((stackOrig == null && stackNew != null) ||
               (stackOrig != null && stackNew != null && stackNew.stackSize == (stackOrig.stackSize + 1)))
            {
                return i;
            }
        }

        return -1;
    }

    private boolean clickSlots(Container container, Minecraft mc, int slotFrom, int slotTo)
    {
        EntityPlayer player = mc.thePlayer;
        //System.out.println("clickSlots(from: " + slotFrom + ", to: " + slotTo + ")");

        ItemStack stack = container.inventorySlots.get(slotFrom).getStack();
        boolean moreThanOne = stack != null && stack.stackSize > 1;

        if (moreThanOne == false)
        {
            // Shift + click the last remaining item
            mc.playerController.windowClick(container.windowId, slotFrom, 0, 1, player);
            return true;
        }

        // Right click on the from-slot to take items to the cursor
        mc.playerController.windowClick(container.windowId, slotFrom, moreThanOne == true ? 1 : 0, 0, player);

        // Right click on the target slot to put one item to it
        mc.playerController.windowClick(container.windowId, slotTo, 1, 0, player);

        // If there are items left in the cursor, then return them back to the original slot
        if (player.inventory.getItemStack() != null)
        {
            // Left click again on the from-slot to return the rest of the items to it
            mc.playerController.windowClick(container.windowId, slotFrom, 0, 0, player);
        }

        return true;
    }
}
