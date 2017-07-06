## Snippets from self-reduction support branch.

## Originally in booking_reductions():


    # Postings held at cost will be classified as either augmentations or
    # reductions. Augmentations will be processed first. The application of
    # reductions is delayed until after.
    reductions = []


            # Resolve reductions to a particular lot in their inventory balance.
            repl_postings = []
            for currency, group_postings in posting_groups:

            ...

                if has_self_reduction(group_postings):
                    # If there's an augmentation and a reduction of the same
                    # commodity in the same currency group, don't allow
                    # interpolation, convert CostSpec to Cost for augmentations,
                    # apply them, and then book reduction. This allows us to reduce
                    # an augmentation that occurs in the same transaction.

                    pass  ## FIXME: TODO

                else:
                    # Otherwise, we can first match the reductions against the
                    # ante-inventory and let the interpolation process take
                    # advantage of any information acquired in doing that.

                    pass  ## FIXME: TODO


## In the augmentation section:

                if 0:
                    # FIXME: There is a problem here... I need to apply the
                    # augmentations in order for the classifications of the
                    # reductions to apply right, but I can't interpolate them this
                    # early. Not sure how to resolve this.

                    # Update the local balance so that postings reducing off
                    # balances added within the same transaction will match.
                    balance.add_position(posting)
