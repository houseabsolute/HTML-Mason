use strict;
use Test::More;
use HTML::Mason;

my $die_at_line4 = <<'';
<%attr>
    key1 => "val1"
</%attr>
% die;

my $die_at_line2 = <<'';
<%attr> key1 => "val1" </%attr>
% die;

{
    my $interp = HTML::Mason::Interp->new;

    my $comp = $interp->make_component( comp_source => $die_at_line4 );
    eval { $interp->exec($comp) };
    like $@, qr/Died at <anonymous component> line 4\./;

    my $source = HTML::Mason::ComponentSource->new(
        friendly_name   => '<anonymous component>',
        comp_path       => '<anonymous component>',
        source_callback => sub { $die_at_line4 },
    );

    my $object_code = $source->object_code(
        compiler  => $interp->compiler,
    );

    like $$object_code, qr/^#line 4 "<anonymous component>"/m;
}

{
    my $interp = HTML::Mason::Interp->new;

    my $comp = $interp->make_component( comp_source => $die_at_line2 );
    eval { $interp->exec($comp) };
    like $@, qr/Died at <anonymous component> line 2\./;

    my $source = HTML::Mason::ComponentSource->new(
        friendly_name   => '<anonymous component>',
        comp_path       => '<anonymous component>',
        source_callback => sub { $die_at_line2 },
    );

    my $object_code = $source->object_code(
        compiler  => $interp->compiler,
    );

    like $$object_code, qr/^#line 2 "<anonymous component>"/m;
}

done_testing;
