{application, simple_whatsapp,
[{vsn, "1.0.0"},
{modules, [simple_whatsapp, chat_sup, chat_process, protocol]},
{env,[{port,8000}]},
{mod, {simple_whatsapp, []}}
]}.