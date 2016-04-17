package fi.dy.masa.itemscroller.event;

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
import net.minecraftforge.items.SlotItemHandler;

import fi.dy.masa.itemscroller.config.Configs;

@SideOnly(Side.CLIENT)
public class InputEventHandler
{
    private boolean leftButtonState;
    private int lastPosX;
    private int lastPosY;
    private int slotNumberLast;
    private ItemStack[] originalStacks = new ItemStack[0];

    @SubscribeEvent
    public void onMouseInputEvent(MouseInputEvent.Pre event)
    {
        int dWheel = Mouse.getEventDWheel();

        if (event.gui instanceof GuiContainer)
        {
            if (dWheel != 0)
            {
                this.tryMoveItems((GuiContainer)event.gui, dWheel > 0);
            }
            else
            {
                this.dragMoveItems((GuiContainer)event.gui);
            }
        }
    }

    private boolean isValidSlotWithItems(Slot slot, GuiContainer gui)
    {
        if (gui.inventorySlots == null || gui.inventorySlots.inventorySlots == null)
        {
            return false;
        }

        return slot != null && gui.inventorySlots.inventorySlots.contains(slot) == true && slot.getHasStack() == true;
    }

    private void dragMoveItems(GuiContainer gui)
    {
        boolean leftButtonPressed = (Mouse.getEventButton() - 100) == gui.mc.gameSettings.keyBindAttack.getKeyCode();
        int mouseX = (Mouse.getEventX() * gui.width / gui.mc.displayWidth) - gui.guiLeft;
        int mouseY = (gui.height - Mouse.getEventY() * gui.height / gui.mc.displayHeight - 1) - gui.guiTop;

        if (leftButtonPressed == true)
        {
            this.leftButtonState = Mouse.getEventButtonState();
        }
        else if (GuiContainer.isShiftKeyDown() == true && this.leftButtonState == true)
        {
            int distX = mouseX - this.lastPosX;
            int distY = mouseY - this.lastPosY;
            int absX = Math.abs(distX);
            int absY = Math.abs(distY);

            if (absX > absY)
            {
                int inc = distX > 0 ? 1 : -1;

                for (int x = this.lastPosX; ; x += inc)
                {
                    int y = absX != 0 ? this.lastPosY + ((x - this.lastPosX) * distY / absX) : mouseY;
                    this.dragMoveFromSlotAtPosition(gui, x, y);

                    if (x == mouseX)
                    {
                        break;
                    }
                }
            }
            else
            {
                int inc = distY > 0 ? 1 : -1;

                for (int y = this.lastPosY; ; y += inc)
                {
                    int x = absY != 0 ? this.lastPosX + ((y - this.lastPosY) * distX / absY) : mouseX;
                    this.dragMoveFromSlotAtPosition(gui, x, y);

                    if (y == mouseY)
                    {
                        break;
                    }
                }
            }
        }

        this.lastPosX = mouseX;
        this.lastPosY = mouseY;
    }

    private void dragMoveFromSlotAtPosition(GuiContainer gui, int x, int y)
    {
        Slot slot = this.getSlotAtPosition(gui, x, y);

        if (slot != null && slot.slotNumber != this.slotNumberLast)
        {
            if (this.isValidSlotWithItems(slot, gui) == true)
            {
                this.shiftClickSlot(gui.inventorySlots, gui.mc, slot.slotNumber);
            }

            this.slotNumberLast = slot.slotNumber;
        }
    }

    private boolean isMouseOverSlot(Slot slot, int mouseX, int mouseY)
    {
        return this.isPointInRegion(slot.xDisplayPosition, slot.yDisplayPosition, 16, 16, mouseX, mouseY);
    }

    private boolean isPointInRegion(int left, int top, int width, int height, int pointX, int pointY)
    {
        return pointX >= left - 1 && pointX < left + width + 1 && pointY >= top - 1 && pointY < top + height + 1;
    }

    private Slot getSlotAtPosition(GuiContainer gui, int x, int y)
    {
        for (int i = 0; i < gui.inventorySlots.inventorySlots.size(); i++)
        {
            Slot slot = gui.inventorySlots.inventorySlots.get(i);

            if (this.isMouseOverSlot(slot, x, y) == true)
            {
                return slot;
            }
        }

        return null;
    }

    private void tryMoveItems(GuiContainer gui, boolean moveToOtherInventory)
    {
        boolean isShiftDown = GuiContainer.isShiftKeyDown();
        boolean isCtrlDown = GuiContainer.isCtrlKeyDown();

        if ((Configs.enableScrollingSingle == false && isShiftDown == false) ||
            (Configs.enableScrollingStacks == false && isShiftDown == true))
        {
            return;
        }

        Slot slot = gui.getSlotUnderMouse();
        if (this.isValidSlotWithItems(slot, gui) == false || gui.mc.thePlayer.inventory.getItemStack() != null)
        {
            return;
        }

        if ((Configs.reverseScrollDirectionSingle == true && isShiftDown == false) ||
            (Configs.reverseScrollDirectionStacks == true && isShiftDown == true))
        {
            moveToOtherInventory = ! moveToOtherInventory;
        }

        if (isShiftDown == true)
        {
            // Ctrl + Shift + scroll: move everything
            if (isCtrlDown == true)
            {
                this.tryMoveStacks(slot, gui, false, moveToOtherInventory, false);
            }
            // Shift + scroll: move one matching stack
            else
            {
                this.tryMoveStacks(slot, gui, true, moveToOtherInventory, true);
            }
        }
        // Ctrl + scroll: Move all matching stacks
        else if (isCtrlDown == true)
        {
            this.tryMoveStacks(slot, gui, true, moveToOtherInventory, false);
        }
        // No Ctrl or Shift
        else
        {
            ItemStack stack = slot.getStack();

            // Scrolling items from this slot/inventory into the other inventory
            if (moveToOtherInventory == true)
            {
                this.tryMoveSingleItemToOtherInventory(slot, gui);
            }
            // Scrolling items from the other inventory into this slot/inventory
            else if (stack.stackSize < slot.getItemStackLimit(stack))
            {
                this.tryMoveSingleItemToThisInventory(slot, gui);
            }
        }
    }

    private void tryMoveSingleItemToOtherInventory(Slot slot, GuiContainer gui)
    {
        if (slot.canTakeStack(gui.mc.thePlayer) == false)
        {
            return;
        }

        ItemStack stackOrig = slot.getStack();
        Container container = gui.inventorySlots;
        ItemStack stack = stackOrig.copy();
        stack.stackSize = 1;

        this.originalStacks = this.getOriginalStacks(container);

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
                this.clickSlotsToMoveSingleItem(container, gui.mc, slot.slotNumber, targetSlot);
                return;
            }
        }

        // Restore the original stack to the slot under the cursor (on the client side)
        slot.putStack(stackOrig);
    }

    private void tryMoveSingleItemToThisInventory(Slot slot, GuiContainer gui)
    {
        Container container = gui.inventorySlots;
        ItemStack stackOrig = slot.getStack();

        for (Slot slotTmp : container.inventorySlots)
        {
            if (areSlotsInSameInventory(slotTmp, slot) == false && (slotTmp instanceof SlotCrafting) == false &&
                slotTmp.getHasStack() == true && slotTmp.canTakeStack(gui.mc.thePlayer) == true)
            {
                ItemStack stackTmp = slotTmp.getStack();
                if (areStacksEqual(stackTmp, stackOrig) == true)
                {
                    this.clickSlotsToMoveSingleItem(container, gui.mc, slotTmp.slotNumber, slot.slotNumber);
                    return;
                }
            }
        }

        // If we weren't able to move any items from another inventory, then try to move items
        // within the same inventory (mostly between the hotbar and the player inventory)
        for (Slot slotTmp : container.inventorySlots)
        {
            if (slotTmp.slotNumber != slot.slotNumber && (slotTmp instanceof SlotCrafting) == false &&
                slotTmp.getHasStack() == true && slotTmp.canTakeStack(gui.mc.thePlayer) == true)
            {
                ItemStack stackTmp = slotTmp.getStack();
                if (areStacksEqual(stackTmp, stackOrig) == true)
                {
                    this.clickSlotsToMoveSingleItem(container, gui.mc, slotTmp.slotNumber, slot.slotNumber);
                    return;
                }
            }
        }
    }

    private void tryMoveStacks(Slot slot, GuiContainer gui, boolean matchingOnly, boolean toOtherInventory, boolean firstOnly)
    {
        Container container = gui.inventorySlots;
        ItemStack stack = slot.getStack();

        for (Slot slotTmp : container.inventorySlots)
        {
            if (slotTmp.slotNumber != slot.slotNumber &&
                areSlotsInSameInventory(slotTmp, slot) == toOtherInventory &&
                (matchingOnly == false || areStacksEqual(stack, slotTmp.getStack()) == true))
            {
                this.shiftClickSlot(container, gui.mc, slotTmp.slotNumber);

                if (firstOnly == true)
                {
                    return;
                }
            }
        }

        // If moving to the other inventory, then move the hovered slot's stack last
        if (toOtherInventory == true)
        {
            this.shiftClickSlot(container, gui.mc, slot.slotNumber);
        }
    }

    private static boolean areStacksEqual(ItemStack stack1, ItemStack stack2)
    {
        return ItemStack.areItemsEqual(stack1, stack2) && ItemStack.areItemStackTagsEqual(stack1, stack2);
    }

    private static boolean areSlotsInSameInventory(Slot slot1, Slot slot2)
    {
        if ((slot1 instanceof SlotItemHandler) && (slot2 instanceof SlotItemHandler))
        {
            return ((SlotItemHandler)slot1).itemHandler == ((SlotItemHandler)slot2).itemHandler;
        }

        return slot1.inventory == slot2.inventory;
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

    private void shiftClickSlot(Container container, Minecraft mc, int slot)
    {
        mc.playerController.windowClick(container.windowId, slot, 0, 1, mc.thePlayer);
    }

    private void clickSlotsToMoveSingleItem(Container container, Minecraft mc, int slotFrom, int slotTo)
    {
        EntityPlayer player = mc.thePlayer;
        //System.out.println("clickSlots(from: " + slotFrom + ", to: " + slotTo + ")");

        ItemStack stack = container.inventorySlots.get(slotFrom).getStack();
        boolean moreThanOne = stack != null && stack.stackSize > 1;

        if (moreThanOne == false)
        {
            // Shift + click the last remaining item
            this.shiftClickSlot(container, mc, slotFrom);
            return;
        }

        // Right click on the from-slot to take items to the cursor. If it's the last item, then left click instead.
        mc.playerController.windowClick(container.windowId, slotFrom, moreThanOne == true ? 1 : 0, 0, player);

        // Right click on the target slot to put one item to it
        mc.playerController.windowClick(container.windowId, slotTo, 1, 0, player);

        // If there are items left in the cursor, then return them back to the original slot
        if (player.inventory.getItemStack() != null)
        {
            // Left click again on the from-slot to return the rest of the items to it
            mc.playerController.windowClick(container.windowId, slotFrom, 0, 0, player);
        }
    }
}
